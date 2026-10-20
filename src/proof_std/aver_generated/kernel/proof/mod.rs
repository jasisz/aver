#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Proof {
    PRefl(std::sync::Arc<Proof__PRefl>),
    PSymm(std::sync::Arc<Proof__PSymm>),
    PTrans(std::sync::Arc<Proof__PTrans>),
    PCongr(std::sync::Arc<Proof__PCongr>),
    PUnfold(std::sync::Arc<Proof__PUnfold>),
    PConst(std::sync::Arc<Proof__PConst>),
    PArm(std::sync::Arc<Proof__PArm>),
    PProj(std::sync::Arc<Proof__PProj>),
    PCell(std::sync::Arc<Proof__PCell>),
    PHyp(std::sync::Arc<Proof__PHyp>),
    PRule(std::sync::Arc<Proof__PRule>),
    PLaw(std::sync::Arc<Proof__PLaw>),
    PCompute(std::sync::Arc<Proof__PCompute>),
    PCases(std::sync::Arc<Proof__PCases>),
    PSplit(std::sync::Arc<Proof__PSplit>),
    PHave(std::sync::Arc<Proof__PHave>),
    PEnum(std::sync::Arc<Proof__PEnum>),
    PAbsurd(std::sync::Arc<Proof__PAbsurd>),
    PInduct(std::sync::Arc<Proof__PInduct>),
    PListInduct(std::sync::Arc<Proof__PListInduct>),
    PListCases(std::sync::Arc<Proof__PListCases>),
    PLenInduct(std::sync::Arc<Proof__PLenInduct>),
    PRing(std::sync::Arc<Proof__PRing>),
    PLinear(std::sync::Arc<Proof__PLinear>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PRefl(pub crate::proof_std::aver_generated::kernel::term::Term);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PSymm(pub Proof);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PTrans(
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<Proof>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PCongr(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PUnfold(
    pub AverStr,
    pub aver_rt::AverInt,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<Proof>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PConst(pub AverStr);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PArm(
    pub aver_rt::AverInt,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PProj(pub crate::proof_std::aver_generated::kernel::term::Term);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PCell(pub crate::proof_std::aver_generated::kernel::term::Term);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PHyp(pub AverStr);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PRule(
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    pub aver_rt::AverList<Proof>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PLaw(
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    pub aver_rt::AverList<Proof>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PCompute(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
);

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PCases(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub AverStr,
    pub Proof,
    pub Proof,
);

impl PartialEq for Proof__PCases {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && self.0 == other.0 && self.2 == other.2 && self.3 == other.3
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PSplit(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<SplitCase>,
);

impl PartialEq for Proof__PSplit {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && self.0 == other.0 && self.2 == other.2 && self.3 == other.3
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PHave(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
    pub Proof,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PEnum(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub aver_rt::AverList<Proof>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PAbsurd(
    pub Proof,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PInduct(
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub aver_rt::AverList<AverStr>,
    pub aver_rt::AverList<Case>,
);

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PListInduct(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub aver_rt::AverList<AverStr>,
    pub Proof,
    pub AverStr,
    pub AverStr,
    pub aver_rt::AverList<AverStr>,
    pub aver_rt::AverList<IhAt>,
    pub Proof,
);

impl PartialEq for Proof__PListInduct {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
            && self.5 == other.5
            && self.6 == other.6
            && self.1 == other.1
            && self.2 == other.2
            && self.3 == other.3
            && self.4 == other.4
            && self.7 == other.7
            && self.8 == other.8
            && self.9 == other.9
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PListCases(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
    pub AverStr,
    pub AverStr,
    pub Proof,
);

impl PartialEq for Proof__PListCases {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
            && self.4 == other.4
            && self.5 == other.5
            && self.1 == other.1
            && self.2 == other.2
            && self.3 == other.3
            && self.6 == other.6
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PLenInduct(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub aver_rt::AverList<AverStr>,
    pub aver_rt::AverList<AverStr>,
    pub AverStr,
    pub AverStr,
    pub AverStr,
    pub Proof,
);

impl PartialEq for Proof__PLenInduct {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
            && self.5 == other.5
            && self.6 == other.6
            && self.7 == other.7
            && self.1 == other.1
            && self.2 == other.2
            && self.3 == other.3
            && self.4 == other.4
            && self.8 == other.8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Proof__PRing(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
);

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PLinear(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub bool,
    pub aver_rt::AverList<AverStr>,
    pub aver_rt::AverIntList,
    pub aver_rt::AverList<Cut>,
);

impl PartialEq for Proof__PLinear {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
            && self.0 == other.0
            && self.2 == other.2
            && self.3 == other.3
            && self.4 == other.4
    }
}

impl Proof {
    fn aver_key_rank(&self) -> usize {
        match self {
            Proof::PAbsurd(..) => 0,
            Proof::PArm(..) => 1,
            Proof::PCases(..) => 2,
            Proof::PCell(..) => 3,
            Proof::PCompute(..) => 4,
            Proof::PCongr(..) => 5,
            Proof::PConst(..) => 6,
            Proof::PEnum(..) => 7,
            Proof::PHave(..) => 8,
            Proof::PHyp(..) => 9,
            Proof::PInduct(..) => 10,
            Proof::PLaw(..) => 11,
            Proof::PLenInduct(..) => 12,
            Proof::PLinear(..) => 13,
            Proof::PListCases(..) => 14,
            Proof::PListInduct(..) => 15,
            Proof::PProj(..) => 16,
            Proof::PRefl(..) => 17,
            Proof::PRing(..) => 18,
            Proof::PRule(..) => 19,
            Proof::PSplit(..) => 20,
            Proof::PSymm(..) => 21,
            Proof::PTrans(..) => 22,
            Proof::PUnfold(..) => 23,
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
            (Proof::PAbsurd(a), Proof::PAbsurd(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            (Proof::PArm(a), Proof::PArm(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            (Proof::PCases(a), Proof::PCases(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            (Proof::PCell(a), Proof::PCell(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Proof::PCompute(a), Proof::PCompute(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Proof::PCongr(a), Proof::PCongr(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Proof::PConst(a), Proof::PConst(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Proof::PEnum(a), Proof::PEnum(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            (Proof::PHave(a), Proof::PHave(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            (Proof::PHyp(a), Proof::PHyp(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Proof::PInduct(a), Proof::PInduct(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5))),
            (Proof::PLaw(a), Proof::PLaw(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            (Proof::PLenInduct(a), Proof::PLenInduct(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5)))
                .then_with(|| (&a.6).cmp((&b.6)))
                .then_with(|| (&a.7).cmp((&b.7)))
                .then_with(|| (&a.8).cmp((&b.8))),
            (Proof::PLinear(a), Proof::PLinear(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4))),
            (Proof::PListCases(a), Proof::PListCases(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5)))
                .then_with(|| (&a.6).cmp((&b.6))),
            (Proof::PListInduct(a), Proof::PListInduct(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5)))
                .then_with(|| (&a.6).cmp((&b.6)))
                .then_with(|| (&a.7).cmp((&b.7)))
                .then_with(|| (&a.8).cmp((&b.8)))
                .then_with(|| (&a.9).cmp((&b.9))),
            (Proof::PProj(a), Proof::PProj(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Proof::PRefl(a), Proof::PRefl(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Proof::PRing(a), Proof::PRing(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Proof::PRule(a), Proof::PRule(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            (Proof::PSplit(a), Proof::PSplit(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            (Proof::PSymm(a), Proof::PSymm(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Proof::PTrans(a), Proof::PTrans(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Proof::PUnfold(a), Proof::PUnfold(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4))),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Proof {
    fn aver_display(&self) -> String {
        match self {
            Proof::PRefl(f) => format!("PRefl({})", (&f.0).aver_display_inner()),
            Proof::PSymm(f) => format!("PSymm({})", (&f.0).aver_display_inner()),
            Proof::PTrans(f) => format!(
                "PTrans({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof::PCongr(f) => format!(
                "PCongr({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof::PUnfold(f) => format!(
                "PUnfold({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PConst(f) => format!("PConst({})", (&f.0).aver_display_inner()),
            Proof::PArm(f) => format!(
                "PArm({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PProj(f) => format!("PProj({})", (&f.0).aver_display_inner()),
            Proof::PCell(f) => format!("PCell({})", (&f.0).aver_display_inner()),
            Proof::PHyp(f) => format!("PHyp({})", (&f.0).aver_display_inner()),
            Proof::PRule(f) => format!(
                "PRule({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PLaw(f) => format!(
                "PLaw({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PCompute(f) => format!(
                "PCompute({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof::PCases(f) => format!(
                "PCases({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PSplit(f) => format!(
                "PSplit({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PHave(f) => format!(
                "PHave({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PEnum(f) => format!(
                "PEnum({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PAbsurd(f) => format!(
                "PAbsurd({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PInduct(f) => format!(
                "PInduct({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner(),
                    (&f.5).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PListInduct(f) => format!(
                "PListInduct({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner(),
                    (&f.5).aver_display_inner(),
                    (&f.6).aver_display_inner(),
                    (&f.7).aver_display_inner(),
                    (&f.8).aver_display_inner(),
                    (&f.9).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PListCases(f) => format!(
                "PListCases({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner(),
                    (&f.5).aver_display_inner(),
                    (&f.6).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PLenInduct(f) => format!(
                "PLenInduct({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner(),
                    (&f.5).aver_display_inner(),
                    (&f.6).aver_display_inner(),
                    (&f.7).aver_display_inner(),
                    (&f.8).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PRing(f) => format!(
                "PRing({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof::PLinear(f) => format!(
                "PLinear({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner()
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
pub struct Cut {
    pub divisor: aver_rt::AverInt,
    pub weights: aver_rt::AverIntList,
}

impl PartialOrd for Cut {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Cut {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.divisor.cmp(&other.divisor))
            .then_with(|| self.weights.cmp(&other.weights))
    }
}

impl aver_rt::AverDisplay for Cut {
    fn aver_display(&self) -> String {
        format!(
            "Cut({})",
            vec![
                format!("divisor: {}", self.divisor.aver_display_inner()),
                format!("weights: {}", self.weights.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Case {
    pub binders: aver_rt::AverList<AverStr>,
    pub ihs: aver_rt::AverList<AverStr>,
    pub carry: aver_rt::AverList<aver_rt::AverList<Proof>>,
    pub more: aver_rt::AverList<More>,
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
            .then_with(|| self.carry.cmp(&other.carry))
            .then_with(|| self.ihs.cmp(&other.ihs))
            .then_with(|| self.more.cmp(&other.more))
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
                format!("carry: {}", self.carry.aver_display_inner()),
                format!("more: {}", self.more.aver_display_inner()),
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
pub struct SplitCase {
    pub ctor: AverStr,
    pub binders: aver_rt::AverList<AverStr>,
    pub proof: Proof,
}

impl PartialOrd for SplitCase {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SplitCase {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.binders.cmp(&other.binders))
            .then_with(|| self.ctor.cmp(&other.ctor))
            .then_with(|| self.proof.cmp(&other.proof))
    }
}

impl aver_rt::AverDisplay for SplitCase {
    fn aver_display(&self) -> String {
        format!(
            "SplitCase({})",
            vec![
                format!("ctor: {}", self.ctor.aver_display_inner()),
                format!("binders: {}", self.binders.aver_display_inner()),
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
pub struct More {
    pub call: aver_rt::AverInt,
    pub ih: IhAt,
}

impl PartialOrd for More {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for More {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.call.cmp(&other.call))
            .then_with(|| self.ih.cmp(&other.ih))
    }
}

impl aver_rt::AverDisplay for More {
    fn aver_display(&self) -> String {
        format!(
            "More({})",
            vec![
                format!("call: {}", self.call.aver_display_inner()),
                format!("ih: {}", self.ih.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IhAt {
    pub name: AverStr,
    pub at: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub carry: aver_rt::AverList<Proof>,
}

impl PartialOrd for IhAt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for IhAt {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.at.cmp(&other.at))
            .then_with(|| self.carry.cmp(&other.carry))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for IhAt {
    fn aver_display(&self) -> String {
        format!(
            "IhAt({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("at: {}", self.at.aver_display_inner()),
                format!("carry: {}", self.carry.aver_display_inner())
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
    pub params: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    pub ret: crate::proof_std::aver_generated::kernel::term::Ty,
    pub lets: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    pub body: crate::proof_std::aver_generated::kernel::term::Term,
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
            .then_with(|| self.ret.cmp(&other.ret))
    }
}

impl aver_rt::AverDisplay for Def {
    fn aver_display(&self) -> String {
        format!(
            "Def({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("params: {}", self.params.aver_display_inner()),
                format!("ret: {}", self.ret.aver_display_inner()),
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
    pub ty: crate::proof_std::aver_generated::kernel::term::Ty,
    pub value: crate::proof_std::aver_generated::kernel::term::Term,
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
            .then_with(|| self.ty.cmp(&other.ty))
            .then_with(|| self.value.cmp(&other.value))
    }
}

impl aver_rt::AverDisplay for Const {
    fn aver_display(&self) -> String {
        format!(
            "Const({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("ty: {}", self.ty.aver_display_inner()),
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
pub struct Variant {
    pub name: AverStr,
    pub fields: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
}

impl PartialOrd for Variant {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Variant {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.fields.cmp(&other.fields))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for Variant {
    fn aver_display(&self) -> String {
        format!(
            "Variant({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("fields: {}", self.fields.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sum {
    pub name: AverStr,
    pub eq: bool,
    pub variants: aver_rt::AverList<Variant>,
}

impl PartialOrd for Sum {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sum {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.eq.cmp(&other.eq))
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.variants.cmp(&other.variants))
    }
}

impl aver_rt::AverDisplay for Sum {
    fn aver_display(&self) -> String {
        format!(
            "Sum({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("eq: {}", self.eq.aver_display_inner()),
                format!("variants: {}", self.variants.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Rec {
    pub name: AverStr,
    pub eq: bool,
    pub fields: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
}

impl PartialOrd for Rec {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rec {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.eq.cmp(&other.eq))
            .then_with(|| self.fields.cmp(&other.fields))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for Rec {
    fn aver_display(&self) -> String {
        format!(
            "Rec({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("eq: {}", self.eq.aver_display_inner()),
                format!("fields: {}", self.fields.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FnSig {
    pub name: AverStr,
    pub params: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    pub ret: crate::proof_std::aver_generated::kernel::term::Ty,
}

impl PartialOrd for FnSig {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnSig {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.params.cmp(&other.params))
            .then_with(|| self.ret.cmp(&other.ret))
    }
}

impl aver_rt::AverDisplay for FnSig {
    fn aver_display(&self) -> String {
        format!(
            "FnSig({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("params: {}", self.params.aver_display_inner()),
                format!("ret: {}", self.ret.aver_display_inner())
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
    pub givens: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    pub premise: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub lhs: crate::proof_std::aver_generated::kernel::term::Term,
    pub rhs: crate::proof_std::aver_generated::kernel::term::Term,
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
pub struct Fact {
    pub law: Law,
    pub proof: Proof,
}

impl PartialOrd for Fact {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fact {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.law.cmp(&other.law))
            .then_with(|| self.proof.cmp(&other.proof))
    }
}

impl aver_rt::AverDisplay for Fact {
    fn aver_display(&self) -> String {
        format!(
            "Fact({})",
            vec![
                format!("law: {}", self.law.aver_display_inner()),
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
pub struct Script {
    pub obligation: Law,
    pub defs: aver_rt::AverList<Def>,
    pub consts: aver_rt::AverList<Const>,
    pub sums: aver_rt::AverList<Sum>,
    pub recs: aver_rt::AverList<Rec>,
    pub sigs: aver_rt::AverList<FnSig>,
    pub laws: aver_rt::AverList<Law>,
    pub facts: aver_rt::AverList<Fact>,
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
            .then_with(|| self.facts.cmp(&other.facts))
            .then_with(|| self.laws.cmp(&other.laws))
            .then_with(|| self.obligation.cmp(&other.obligation))
            .then_with(|| self.proof.cmp(&other.proof))
            .then_with(|| self.recs.cmp(&other.recs))
            .then_with(|| self.sigs.cmp(&other.sigs))
            .then_with(|| self.sums.cmp(&other.sums))
    }
}

impl aver_rt::AverDisplay for Script {
    fn aver_display(&self) -> String {
        format!(
            "Script({})",
            vec![
                format!("obligation: {}", self.obligation.aver_display_inner()),
                format!("defs: {}", self.defs.aver_display_inner()),
                format!("consts: {}", self.consts.aver_display_inner()),
                format!("sums: {}", self.sums.aver_display_inner()),
                format!("recs: {}", self.recs.aver_display_inner()),
                format!("sigs: {}", self.sigs.aver_display_inner()),
                format!("laws: {}", self.laws.aver_display_inner()),
                format!("facts: {}", self.facts.aver_display_inner()),
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
pub struct Given {
    pub name: AverStr,
    pub fin: crate::proof_std::aver_generated::kernel::term::Fin,
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
pub struct Goal {
    pub obligation: Law,
    pub finite: aver_rt::AverList<Given>,
    pub lists: aver_rt::AverList<AverStr>,
    pub ints: aver_rt::AverList<AverStr>,
    pub defs: aver_rt::AverList<Def>,
    pub consts: aver_rt::AverList<Const>,
    pub sums: aver_rt::AverList<Sum>,
    pub recs: aver_rt::AverList<Rec>,
    pub laws: aver_rt::AverList<Law>,
    pub facts: aver_rt::AverList<Fact>,
    pub hyps: aver_rt::AverList<Hyp>,
}

impl PartialOrd for Goal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Goal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.consts.cmp(&other.consts))
            .then_with(|| self.defs.cmp(&other.defs))
            .then_with(|| self.facts.cmp(&other.facts))
            .then_with(|| self.finite.cmp(&other.finite))
            .then_with(|| self.hyps.cmp(&other.hyps))
            .then_with(|| self.ints.cmp(&other.ints))
            .then_with(|| self.laws.cmp(&other.laws))
            .then_with(|| self.lists.cmp(&other.lists))
            .then_with(|| self.obligation.cmp(&other.obligation))
            .then_with(|| self.recs.cmp(&other.recs))
            .then_with(|| self.sums.cmp(&other.sums))
    }
}

impl aver_rt::AverDisplay for Goal {
    fn aver_display(&self) -> String {
        format!(
            "Goal({})",
            vec![
                format!("obligation: {}", self.obligation.aver_display_inner()),
                format!("finite: {}", self.finite.aver_display_inner()),
                format!("lists: {}", self.lists.aver_display_inner()),
                format!("ints: {}", self.ints.aver_display_inner()),
                format!("defs: {}", self.defs.aver_display_inner()),
                format!("consts: {}", self.consts.aver_display_inner()),
                format!("sums: {}", self.sums.aver_display_inner()),
                format!("recs: {}", self.recs.aver_display_inner()),
                format!("laws: {}", self.laws.aver_display_inner()),
                format!("facts: {}", self.facts.aver_display_inner()),
                format!("hyps: {}", self.hyps.aver_display_inner())
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
    pub eqn: crate::proof_std::aver_generated::kernel::term::Eqn,
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
pub fn premiseOf(law @ _: &Law) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(law.premise.clone(), [] => None, [p, rest] => Some(p))
}

/// The names of typed variables, in order.
#[inline(always)]
pub fn givenNames(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(gs.clone(), [] => aver_rt::AverList::empty(), [g, rest] => aver_rt::AverList::prepend(g.name, &crate::proof_std::aver_generated::kernel::proof::givenNames(&rest)))
}

/// A definition by name.
#[inline(always)]
pub fn findDef(mut name @ _: AverStr, mut ds @ _: aver_rt::AverList<Def>) -> Option<Def> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ds, [] => { return None; }, [d, rest] => { if (d.name == name) { return Some(d); } else { {
            let __tco1 = rest;
            ds = __tco1;
            continue;
        } } })
    }
}

/// The constructors of a sum, in order.
#[inline(always)]
pub fn variantNames(vs @ _: &aver_rt::AverList<Variant>) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(vs.clone(), [] => aver_rt::AverList::empty(), [v, rest] => aver_rt::AverList::prepend(v.name, &crate::proof_std::aver_generated::kernel::proof::variantNames(&rest)))
}
