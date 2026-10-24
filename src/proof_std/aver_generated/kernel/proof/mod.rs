#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

::aver_rt::thin_sum! {
    #[derive(Debug)]
    pub Proof / Proof__view / Proof__pair {
        0 PRefl(Proof__PRefl),
        1 PSymm(Proof__PSymm),
        2 PTrans(Proof__PTrans),
        3 PCongr(Proof__PCongr),
        4 PUnfold(Proof__PUnfold),
        5 PConst(Proof__PConst),
        6 PArm(Proof__PArm),
        7 PProj(Proof__PProj),
        8 PCell(Proof__PCell),
        9 PHyp(Proof__PHyp),
        10 PRule(Proof__PRule),
        11 PLaw(Proof__PLaw),
        12 PCompute(Proof__PCompute),
        13 PCases(Proof__PCases),
        14 PSplit(Proof__PSplit),
        15 PHave(Proof__PHave),
        16 PEnum(Proof__PEnum),
        17 PAbsurd(Proof__PAbsurd),
        18 PInduct(Proof__PInduct),
        19 PListInduct(Proof__PListInduct),
        20 PListCases(Proof__PListCases),
        21 PLenInduct(Proof__PLenInduct),
        22 PRing(Proof__PRing),
        23 PLinear(Proof__PLinear),
    }
}

impl std::fmt::Debug for Proof {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.view(), f)
    }
}

impl std::hash::Hash for Proof {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Proof {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self.view() {
            Proof__view::PRefl(f) => {
                std::hash::Hash::hash(&0u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Proof__view::PSymm(f) => {
                std::hash::Hash::hash(&1u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Proof__view::PTrans(f) => {
                std::hash::Hash::hash(&2u32, state);
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                left
            }
            Proof__view::PCongr(f) => {
                std::hash::Hash::hash(&3u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                left
            }
            Proof__view::PUnfold(f) => {
                std::hash::Hash::hash(&4u32, state);
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                std::hash::Hash::hash(&f.3, state);
                std::hash::Hash::hash(&f.4, state);
                left
            }
            Proof__view::PConst(f) => {
                std::hash::Hash::hash(&5u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Proof__view::PArm(f) => {
                std::hash::Hash::hash(&6u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                left
            }
            Proof__view::PProj(f) => {
                std::hash::Hash::hash(&7u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Proof__view::PCell(f) => {
                std::hash::Hash::hash(&8u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Proof__view::PHyp(f) => {
                std::hash::Hash::hash(&9u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Proof__view::PRule(f) => {
                std::hash::Hash::hash(&10u32, state);
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                left
            }
            Proof__view::PLaw(f) => {
                std::hash::Hash::hash(&11u32, state);
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                left
            }
            Proof__view::PCompute(f) => {
                std::hash::Hash::hash(&12u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                left
            }
            Proof__view::PCases(f) => {
                std::hash::Hash::hash(&13u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                std::hash::Hash::hash(&f.1, state);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                left
            }
            Proof__view::PSplit(f) => {
                std::hash::Hash::hash(&14u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                std::hash::Hash::hash(&f.3, state);
                left
            }
            Proof__view::PHave(f) => {
                std::hash::Hash::hash(&15u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                left
            }
            Proof__view::PEnum(f) => {
                std::hash::Hash::hash(&16u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                std::hash::Hash::hash(&f.3, state);
                left
            }
            Proof__view::PAbsurd(f) => {
                std::hash::Hash::hash(&17u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                left
            }
            Proof__view::PInduct(f) => {
                std::hash::Hash::hash(&18u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                std::hash::Hash::hash(&f.4, state);
                std::hash::Hash::hash(&f.5, state);
                left
            }
            Proof__view::PListInduct(f) => {
                std::hash::Hash::hash(&19u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                std::hash::Hash::hash(&f.3, state);
                let left = aver_rt::WalkHash::stream(&f.4, state, left);
                std::hash::Hash::hash(&f.5, state);
                std::hash::Hash::hash(&f.6, state);
                std::hash::Hash::hash(&f.7, state);
                std::hash::Hash::hash(&f.8, state);
                let left = aver_rt::WalkHash::stream(&f.9, state, left);
                left
            }
            Proof__view::PListCases(f) => {
                std::hash::Hash::hash(&20u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                std::hash::Hash::hash(&f.4, state);
                std::hash::Hash::hash(&f.5, state);
                let left = aver_rt::WalkHash::stream(&f.6, state, left);
                left
            }
            Proof__view::PLenInduct(f) => {
                std::hash::Hash::hash(&21u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                std::hash::Hash::hash(&f.3, state);
                std::hash::Hash::hash(&f.4, state);
                std::hash::Hash::hash(&f.5, state);
                std::hash::Hash::hash(&f.6, state);
                std::hash::Hash::hash(&f.7, state);
                let left = aver_rt::WalkHash::stream(&f.8, state, left);
                left
            }
            Proof__view::PRing(f) => {
                std::hash::Hash::hash(&22u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                left
            }
            Proof__view::PLinear(f) => {
                std::hash::Hash::hash(&23u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                std::hash::Hash::hash(&f.3, state);
                std::hash::Hash::hash(&f.4, state);
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self.view() {
            Proof__view::PRefl(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&0u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Proof__view::PSymm(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&1u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Proof__view::PTrans(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&2u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
            }),
            Proof__view::PCongr(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&3u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
            }),
            Proof__view::PUnfold(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&4u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
                std::hash::Hash::hash(&f.3, h);
                std::hash::Hash::hash(&f.4, h);
            }),
            Proof__view::PConst(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&5u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Proof__view::PArm(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&6u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
            }),
            Proof__view::PProj(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&7u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Proof__view::PCell(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&8u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Proof__view::PHyp(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&9u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Proof__view::PRule(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&10u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
            }),
            Proof__view::PLaw(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&11u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
            }),
            Proof__view::PCompute(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&12u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
            }),
            Proof__view::PCases(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&13u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
            }),
            Proof__view::PSplit(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&14u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
                std::hash::Hash::hash(&f.3, h);
            }),
            Proof__view::PHave(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&15u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
            }),
            Proof__view::PEnum(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&16u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hash::hash(&f.3, h);
            }),
            Proof__view::PAbsurd(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&17u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
            }),
            Proof__view::PInduct(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&18u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
                std::hash::Hash::hash(&f.4, h);
                std::hash::Hash::hash(&f.5, h);
            }),
            Proof__view::PListInduct(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&19u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hash::hash(&f.3, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.4, walk));
                std::hash::Hash::hash(&f.5, h);
                std::hash::Hash::hash(&f.6, h);
                std::hash::Hash::hash(&f.7, h);
                std::hash::Hash::hash(&f.8, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.9, walk));
            }),
            Proof__view::PListCases(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&20u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
                std::hash::Hash::hash(&f.4, h);
                std::hash::Hash::hash(&f.5, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.6, walk));
            }),
            Proof__view::PLenInduct(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&21u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
                std::hash::Hash::hash(&f.3, h);
                std::hash::Hash::hash(&f.4, h);
                std::hash::Hash::hash(&f.5, h);
                std::hash::Hash::hash(&f.6, h);
                std::hash::Hash::hash(&f.7, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.8, walk));
            }),
            Proof__view::PRing(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&22u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
            }),
            Proof__view::PLinear(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&23u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
                std::hash::Hash::hash(&f.3, h);
                std::hash::Hash::hash(&f.4, h);
            }),
        }
    }
}

impl Eq for Proof {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PRefl(pub crate::proof_std::aver_generated::kernel::term::Term);

impl PartialEq for Proof__PRefl {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Proof__PRefl {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PRefl {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PSymm(pub Proof);

impl PartialEq for Proof__PSymm {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Proof__PSymm {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PSymm {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PTrans(
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<Proof>,
);

impl PartialEq for Proof__PTrans {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::thin::HandOver for Proof__PTrans {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PCongr(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
);

impl PartialEq for Proof__PCongr {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Proof__PCongr {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PCongr {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PUnfold(
    pub AverStr,
    pub aver_rt::AverInt,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub aver_rt::AverList<Proof>,
);

impl PartialEq for Proof__PUnfold {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0
                && self.1 == other.1
                && self.2 == other.2
                && self.3 == other.3
                && self.4 == other.4
    }
}

impl aver_rt::thin::HandOver for Proof__PUnfold {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PConst(pub AverStr);

impl PartialEq for Proof__PConst {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Proof__PConst {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PArm(
    pub aver_rt::AverInt,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
);

impl PartialEq for Proof__PArm {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0 && self.1 == other.1 && self.2 == other.2 && self.3 == other.3
    }
}

impl aver_rt::WalkEq for Proof__PArm {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1 == other.1
            && self.2.walk_eq(&other.2, walk)
            && self.3.walk_eq(&other.3, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PArm {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.3, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PProj(pub crate::proof_std::aver_generated::kernel::term::Term);

impl PartialEq for Proof__PProj {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Proof__PProj {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PProj {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PCell(pub crate::proof_std::aver_generated::kernel::term::Term);

impl PartialEq for Proof__PCell {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Proof__PCell {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PCell {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PHyp(pub AverStr);

impl PartialEq for Proof__PHyp {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Proof__PHyp {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PRule(
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    pub aver_rt::AverList<Proof>,
);

impl PartialEq for Proof__PRule {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1 && self.2 == other.2
    }
}

impl aver_rt::thin::HandOver for Proof__PRule {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PLaw(
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    pub aver_rt::AverList<Proof>,
);

impl PartialEq for Proof__PLaw {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1 && self.2 == other.2
    }
}

impl aver_rt::thin::HandOver for Proof__PLaw {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PCompute(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
);

impl PartialEq for Proof__PCompute {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Proof__PCompute {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PCompute {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PCases(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub AverStr,
    pub Proof,
    pub Proof,
);

impl PartialEq for Proof__PCases {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.1 == other.1 && self.0 == other.0 && self.2 == other.2 && self.3 == other.3
    }
}

impl aver_rt::WalkEq for Proof__PCases {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.1 == other.1
            && self.0.walk_eq(&other.0, walk)
            && self.2.walk_eq(&other.2, walk)
            && self.3.walk_eq(&other.3, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PCases {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.3, pending);
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
        std::ptr::eq(self, other)
            || self.1 == other.1 && self.0 == other.0 && self.2 == other.2 && self.3 == other.3
    }
}

impl aver_rt::WalkEq for Proof__PSplit {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.1 == other.1
            && self.0.walk_eq(&other.0, walk)
            && self.2 == other.2
            && self.3 == other.3
    }
}

impl aver_rt::thin::HandOver for Proof__PSplit {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PHave(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Proof,
    pub Proof,
);

impl PartialEq for Proof__PHave {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0 && self.1 == other.1 && self.2 == other.2 && self.3 == other.3
    }
}

impl aver_rt::WalkEq for Proof__PHave {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
            && self.3.walk_eq(&other.3, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PHave {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.3, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PEnum(
    pub AverStr,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub aver_rt::AverList<Proof>,
);

impl PartialEq for Proof__PEnum {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0 && self.1 == other.1 && self.2 == other.2 && self.3 == other.3
    }
}

impl aver_rt::WalkEq for Proof__PEnum {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
            && self.3 == other.3
    }
}

impl aver_rt::thin::HandOver for Proof__PEnum {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PAbsurd(
    pub Proof,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
);

impl PartialEq for Proof__PAbsurd {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1 && self.2 == other.2
    }
}

impl aver_rt::WalkEq for Proof__PAbsurd {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PAbsurd {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PInduct(
    pub AverStr,
    pub aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub aver_rt::AverList<AverStr>,
    pub aver_rt::AverList<Case>,
);

impl PartialEq for Proof__PInduct {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0
                && self.1 == other.1
                && self.2 == other.2
                && self.3 == other.3
                && self.4 == other.4
                && self.5 == other.5
    }
}

impl aver_rt::WalkEq for Proof__PInduct {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1 == other.1
            && self.2.walk_eq(&other.2, walk)
            && self.3.walk_eq(&other.3, walk)
            && self.4 == other.4
            && self.5 == other.5
    }
}

impl aver_rt::thin::HandOver for Proof__PInduct {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.3, pending);
    }
}

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
        std::ptr::eq(self, other)
            || self.0 == other.0
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

impl aver_rt::WalkEq for Proof__PListInduct {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.5 == other.5
            && self.6 == other.6
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
            && self.3 == other.3
            && self.4.walk_eq(&other.4, walk)
            && self.7 == other.7
            && self.8 == other.8
            && self.9.walk_eq(&other.9, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PListInduct {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.4, pending);
        aver_rt::thin::HandOver::hand_over(self.9, pending);
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
        std::ptr::eq(self, other)
            || self.0 == other.0
                && self.4 == other.4
                && self.5 == other.5
                && self.1 == other.1
                && self.2 == other.2
                && self.3 == other.3
                && self.6 == other.6
    }
}

impl aver_rt::WalkEq for Proof__PListCases {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.4 == other.4
            && self.5 == other.5
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
            && self.3.walk_eq(&other.3, walk)
            && self.6.walk_eq(&other.6, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PListCases {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.3, pending);
        aver_rt::thin::HandOver::hand_over(self.6, pending);
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
        std::ptr::eq(self, other)
            || self.0 == other.0
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

impl aver_rt::WalkEq for Proof__PLenInduct {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.5 == other.5
            && self.6 == other.6
            && self.7 == other.7
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
            && self.3 == other.3
            && self.4 == other.4
            && self.8.walk_eq(&other.8, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PLenInduct {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
        aver_rt::thin::HandOver::hand_over(self.8, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Proof__PRing(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub crate::proof_std::aver_generated::kernel::term::Term,
);

impl PartialEq for Proof__PRing {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Proof__PRing {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Proof__PRing {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

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
        std::ptr::eq(self, other)
            || self.1 == other.1
                && self.0 == other.0
                && self.2 == other.2
                && self.3 == other.3
                && self.4 == other.4
    }
}

impl aver_rt::WalkEq for Proof__PLinear {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.1 == other.1
            && self.0.walk_eq(&other.0, walk)
            && self.2 == other.2
            && self.3 == other.3
            && self.4 == other.4
    }
}

impl aver_rt::thin::HandOver for Proof__PLinear {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

impl aver_rt::WalkEq for Proof {
    #[inline(always)]
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        Proof::ptr_eq(self, other) || self.aver_walk_eq_nodes(other, walk)
    }
}

impl Proof {
    #[inline(never)]
    fn aver_walk_eq_nodes<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        use ::aver_rt::WalkEq as _;
        match Proof::view_pair(self, other) {
            Some(Proof__pair::PRefl(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PSymm(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PTrans(a0, b0)) => a0 == b0,
            Some(Proof__pair::PCongr(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PUnfold(a0, b0)) => a0 == b0,
            Some(Proof__pair::PConst(a0, b0)) => a0 == b0,
            Some(Proof__pair::PArm(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PProj(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PCell(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PHyp(a0, b0)) => a0 == b0,
            Some(Proof__pair::PRule(a0, b0)) => a0 == b0,
            Some(Proof__pair::PLaw(a0, b0)) => a0 == b0,
            Some(Proof__pair::PCompute(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PCases(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PSplit(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PHave(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PEnum(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PAbsurd(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PInduct(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PListInduct(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PListCases(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PLenInduct(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PRing(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Proof__pair::PLinear(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            None => false,
        }
    }
}

impl PartialEq for Proof {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::walk_eq(self, other)
    }
}

impl Proof {
    fn aver_key_rank(&self) -> usize {
        match self.view() {
            Proof__view::PAbsurd(..) => 0,
            Proof__view::PArm(..) => 1,
            Proof__view::PCases(..) => 2,
            Proof__view::PCell(..) => 3,
            Proof__view::PCompute(..) => 4,
            Proof__view::PCongr(..) => 5,
            Proof__view::PConst(..) => 6,
            Proof__view::PEnum(..) => 7,
            Proof__view::PHave(..) => 8,
            Proof__view::PHyp(..) => 9,
            Proof__view::PInduct(..) => 10,
            Proof__view::PLaw(..) => 11,
            Proof__view::PLenInduct(..) => 12,
            Proof__view::PLinear(..) => 13,
            Proof__view::PListCases(..) => 14,
            Proof__view::PListInduct(..) => 15,
            Proof__view::PProj(..) => 16,
            Proof__view::PRefl(..) => 17,
            Proof__view::PRing(..) => 18,
            Proof__view::PRule(..) => 19,
            Proof__view::PSplit(..) => 20,
            Proof__view::PSymm(..) => 21,
            Proof__view::PTrans(..) => 22,
            Proof__view::PUnfold(..) => 23,
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
        match Proof::view_pair(self, other) {
            Some(Proof__pair::PAbsurd(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            Some(Proof__pair::PArm(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            Some(Proof__pair::PCases(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            Some(Proof__pair::PCell(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Proof__pair::PCompute(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Proof__pair::PCongr(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Proof__pair::PConst(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Proof__pair::PEnum(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            Some(Proof__pair::PHave(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            Some(Proof__pair::PHyp(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Proof__pair::PInduct(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5))),
            Some(Proof__pair::PLaw(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            Some(Proof__pair::PLenInduct(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5)))
                .then_with(|| (&a.6).cmp((&b.6)))
                .then_with(|| (&a.7).cmp((&b.7)))
                .then_with(|| (&a.8).cmp((&b.8))),
            Some(Proof__pair::PLinear(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4))),
            Some(Proof__pair::PListCases(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4)))
                .then_with(|| (&a.5).cmp((&b.5)))
                .then_with(|| (&a.6).cmp((&b.6))),
            Some(Proof__pair::PListInduct(a, b)) => std::cmp::Ordering::Equal
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
            Some(Proof__pair::PProj(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Proof__pair::PRefl(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Proof__pair::PRing(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Proof__pair::PRule(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            Some(Proof__pair::PSplit(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3))),
            Some(Proof__pair::PSymm(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Proof__pair::PTrans(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Proof__pair::PUnfold(a, b)) => std::cmp::Ordering::Equal
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
        match self.view() {
            Proof__view::PRefl(f) => format!("PRefl({})", (&f.0).aver_display_inner()),
            Proof__view::PSymm(f) => format!("PSymm({})", (&f.0).aver_display_inner()),
            Proof__view::PTrans(f) => format!(
                "PTrans({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof__view::PCongr(f) => format!(
                "PCongr({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof__view::PUnfold(f) => format!(
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
            Proof__view::PConst(f) => format!("PConst({})", (&f.0).aver_display_inner()),
            Proof__view::PArm(f) => format!(
                "PArm({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PProj(f) => format!("PProj({})", (&f.0).aver_display_inner()),
            Proof__view::PCell(f) => format!("PCell({})", (&f.0).aver_display_inner()),
            Proof__view::PHyp(f) => format!("PHyp({})", (&f.0).aver_display_inner()),
            Proof__view::PRule(f) => format!(
                "PRule({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PLaw(f) => format!(
                "PLaw({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PCompute(f) => format!(
                "PCompute({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof__view::PCases(f) => format!(
                "PCases({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PSplit(f) => format!(
                "PSplit({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PHave(f) => format!(
                "PHave({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PEnum(f) => format!(
                "PEnum({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PAbsurd(f) => format!(
                "PAbsurd({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Proof__view::PInduct(f) => format!(
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
            Proof__view::PListInduct(f) => format!(
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
            Proof__view::PListCases(f) => format!(
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
            Proof__view::PLenInduct(f) => format!(
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
            Proof__view::PRing(f) => format!(
                "PRing({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Proof__view::PLinear(f) => format!(
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

pub type Script = Box<Script__Fields>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Script__Fields {
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

impl PartialOrd for Script__Fields {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Script__Fields {
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

impl aver_rt::AverDisplay for Script__Fields {
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

pub type Goal = Box<Goal__Fields>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Goal__Fields {
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

impl PartialOrd for Goal__Fields {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Goal__Fields {
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

impl aver_rt::AverDisplay for Goal__Fields {
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
    {
        let __list_subject = law.premise.clone();
        if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            Some(p.clone())
        } else {
            if __list_subject.is_empty() {
                None
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The names of typed variables, in order.
#[inline(always)]
pub fn givenNames(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = gs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    g.name.clone(),
                    crate::proof_std::aver_generated::kernel::proof::givenNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A definition by name.
#[inline(always)]
pub fn findDef(mut name @ _: AverStr, ds @ _: &aver_rt::AverList<Def>) -> Option<Def> {
    let mut __hold1: aver_rt::AverList<Def>;
    let mut ds @ _: &aver_rt::AverList<Def> = ds;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ds;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(d), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (d.name == name) {
                        return Some(d.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            ds = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The constructors of a sum, in order.
#[inline(always)]
pub fn variantNames(vs @ _: &aver_rt::AverList<Variant>) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = vs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    v.name.clone(),
                    crate::proof_std::aver_generated::kernel::proof::variantNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}
