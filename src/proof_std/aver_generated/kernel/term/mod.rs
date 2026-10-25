#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

::aver_rt::thin_sum! {
    #[derive(Debug)]
    pub Ty / Ty__view / Ty__pair {
        0 TyInt,
        1 TyBool,
        2 TyStr,
        3 TyFloat,
        4 TyUnit,
        5 TyList(Ty__TyList),
        6 TyOption(Ty__TyOption),
        7 TyResult(Ty__TyResult),
        8 TyMap(Ty__TyMap),
        9 TyVector(Ty__TyVector),
        10 TyTuple(Ty__TyTuple),
        11 TyNamed(Ty__TyNamed),
        12 TyAny,
        13 TyVar(Ty__TyVar),
        14 TyRigid(Ty__TyRigid),
    }
}

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.view(), f)
    }
}

impl std::hash::Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Ty {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self.view() {
            Ty__view::TyInt => {
                std::hash::Hash::hash(&0u32, state);
                left
            }
            Ty__view::TyBool => {
                std::hash::Hash::hash(&1u32, state);
                left
            }
            Ty__view::TyStr => {
                std::hash::Hash::hash(&2u32, state);
                left
            }
            Ty__view::TyFloat => {
                std::hash::Hash::hash(&3u32, state);
                left
            }
            Ty__view::TyUnit => {
                std::hash::Hash::hash(&4u32, state);
                left
            }
            Ty__view::TyList(f) => {
                std::hash::Hash::hash(&5u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Ty__view::TyOption(f) => {
                std::hash::Hash::hash(&6u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Ty__view::TyResult(f) => {
                std::hash::Hash::hash(&7u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                left
            }
            Ty__view::TyMap(f) => {
                std::hash::Hash::hash(&8u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                left
            }
            Ty__view::TyVector(f) => {
                std::hash::Hash::hash(&9u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Ty__view::TyTuple(f) => {
                std::hash::Hash::hash(&10u32, state);
                let left = aver_rt::shared_items::stream_items(
                    f.0.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Ty__view::TyNamed(f) => {
                std::hash::Hash::hash(&11u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Ty__view::TyAny => {
                std::hash::Hash::hash(&12u32, state);
                left
            }
            Ty__view::TyVar(f) => {
                std::hash::Hash::hash(&13u32, state);
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                left
            }
            Ty__view::TyRigid(f) => {
                std::hash::Hash::hash(&14u32, state);
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self.view() {
            Ty__view::TyInt => aver_rt::shared_hash::tag_hash(0),
            Ty__view::TyBool => aver_rt::shared_hash::tag_hash(1),
            Ty__view::TyStr => aver_rt::shared_hash::tag_hash(2),
            Ty__view::TyFloat => aver_rt::shared_hash::tag_hash(3),
            Ty__view::TyUnit => aver_rt::shared_hash::tag_hash(4),
            Ty__view::TyList(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&5u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Ty__view::TyOption(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&6u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Ty__view::TyResult(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&7u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
            }),
            Ty__view::TyMap(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&8u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
            }),
            Ty__view::TyVector(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&9u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Ty__view::TyTuple(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&10u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.0, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Ty__view::TyNamed(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&11u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Ty__view::TyAny => aver_rt::shared_hash::tag_hash(12),
            Ty__view::TyVar(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&13u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
            }),
            Ty__view::TyRigid(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&14u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
            }),
        }
    }
}

impl Eq for Ty {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyList(pub Ty);

impl PartialEq for Ty__TyList {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Ty__TyList {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Ty__TyList {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyOption(pub Ty);

impl PartialEq for Ty__TyOption {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Ty__TyOption {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Ty__TyOption {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyResult(pub Ty, pub Ty);

impl PartialEq for Ty__TyResult {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Ty__TyResult {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Ty__TyResult {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyMap(pub Ty, pub Ty);

impl PartialEq for Ty__TyMap {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Ty__TyMap {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Ty__TyMap {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyVector(pub Ty);

impl PartialEq for Ty__TyVector {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Ty__TyVector {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Ty__TyVector {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyTuple(pub aver_rt::AverList<Ty>);

impl PartialEq for Ty__TyTuple {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Ty__TyTuple {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        aver_rt::shared_items::eq_list(&self.0, &other.0, walk, |x, y, walk| {
            (*x).walk_eq(&(*y), walk)
        })
    }
}

impl aver_rt::thin::HandOver for Ty__TyTuple {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyNamed(pub AverStr);

impl PartialEq for Ty__TyNamed {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Ty__TyNamed {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyVar(pub AverStr, pub bool);

impl PartialEq for Ty__TyVar {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::thin::HandOver for Ty__TyVar {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Ty__TyRigid(pub AverStr, pub bool);

impl PartialEq for Ty__TyRigid {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::thin::HandOver for Ty__TyRigid {}

impl aver_rt::WalkEq for Ty {
    #[inline(always)]
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        Ty::ptr_eq(self, other) || self.aver_walk_eq_nodes(other, walk)
    }
}

impl Ty {
    #[inline(never)]
    fn aver_walk_eq_nodes<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        use ::aver_rt::WalkEq as _;
        match Ty::view_pair(self, other) {
            Some(Ty__pair::TyInt) => true,
            Some(Ty__pair::TyBool) => true,
            Some(Ty__pair::TyStr) => true,
            Some(Ty__pair::TyFloat) => true,
            Some(Ty__pair::TyUnit) => true,
            Some(Ty__pair::TyList(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Ty__pair::TyOption(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Ty__pair::TyResult(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Ty__pair::TyMap(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Ty__pair::TyVector(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Ty__pair::TyTuple(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Ty__pair::TyNamed(a0, b0)) => a0 == b0,
            Some(Ty__pair::TyAny) => true,
            Some(Ty__pair::TyVar(a0, b0)) => a0 == b0,
            Some(Ty__pair::TyRigid(a0, b0)) => a0 == b0,
            None => false,
        }
    }
}

impl PartialEq for Ty {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::walk_eq(self, other)
    }
}

impl Ty {
    fn aver_key_rank(&self) -> usize {
        match self.view() {
            Ty__view::TyAny => 0,
            Ty__view::TyBool => 1,
            Ty__view::TyFloat => 2,
            Ty__view::TyInt => 3,
            Ty__view::TyList(..) => 4,
            Ty__view::TyMap(..) => 5,
            Ty__view::TyNamed(..) => 6,
            Ty__view::TyOption(..) => 7,
            Ty__view::TyResult(..) => 8,
            Ty__view::TyRigid(..) => 9,
            Ty__view::TyStr => 10,
            Ty__view::TyTuple(..) => 11,
            Ty__view::TyUnit => 12,
            Ty__view::TyVar(..) => 13,
            Ty__view::TyVector(..) => 14,
        }
    }
}

impl PartialOrd for Ty {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ty {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match Ty::view_pair(self, other) {
            Some(Ty__pair::TyList(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Ty__pair::TyMap(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Ty__pair::TyNamed(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Ty__pair::TyOption(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Ty__pair::TyResult(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Ty__pair::TyRigid(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Ty__pair::TyTuple(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Ty__pair::TyVar(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Ty__pair::TyVector(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Ty {
    fn aver_display(&self) -> String {
        match self.view() {
            Ty__view::TyInt => "TyInt".to_string(),
            Ty__view::TyBool => "TyBool".to_string(),
            Ty__view::TyStr => "TyStr".to_string(),
            Ty__view::TyFloat => "TyFloat".to_string(),
            Ty__view::TyUnit => "TyUnit".to_string(),
            Ty__view::TyList(f) => format!("TyList({})", (&f.0).aver_display_inner()),
            Ty__view::TyOption(f) => format!("TyOption({})", (&f.0).aver_display_inner()),
            Ty__view::TyResult(f) => format!(
                "TyResult({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Ty__view::TyMap(f) => format!(
                "TyMap({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Ty__view::TyVector(f) => format!("TyVector({})", (&f.0).aver_display_inner()),
            Ty__view::TyTuple(f) => format!("TyTuple({})", (&f.0).aver_display_inner()),
            Ty__view::TyNamed(f) => format!("TyNamed({})", (&f.0).aver_display_inner()),
            Ty__view::TyAny => "TyAny".to_string(),
            Ty__view::TyVar(f) => format!(
                "TyVar({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Ty__view::TyRigid(f) => format!(
                "TyRigid({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedVar {
    pub name: AverStr,
    pub ty: Ty,
}

impl PartialOrd for TypedVar {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TypedVar {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.ty.cmp(&other.ty))
    }
}

impl aver_rt::AverDisplay for TypedVar {
    fn aver_display(&self) -> String {
        format!(
            "TypedVar({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("ty: {}", self.ty.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

::aver_rt::thin_sum! {
    #[derive(Debug)]
    pub Term / Term__view / Term__pair {
        0 TInt(Term__TInt),
        1 TBool(Term__TBool),
        2 TStr(Term__TStr),
        3 TUnit,
        4 TVar(Term__TVar),
        5 THole,
        6 TGet(Term__TGet),
        7 TCall(Term__TCall),
        8 TBi(Term__TBi),
        9 TOp(Term__TOp),
        10 TNeg(Term__TNeg),
        11 TCtor(Term__TCtor),
        12 TMatch(Term__TMatch),
        13 TParts(Term__TParts),
        14 TList(Term__TList),
        15 TTuple(Term__TTuple),
        16 TRec(Term__TRec),
        17 TUpd(Term__TUpd),
    }
}

impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.view(), f)
    }
}

impl std::hash::Hash for Term {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Term {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self.view() {
            Term__view::TInt(f) => {
                std::hash::Hash::hash(&0u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Term__view::TBool(f) => {
                std::hash::Hash::hash(&1u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Term__view::TStr(f) => {
                std::hash::Hash::hash(&2u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Term__view::TUnit => {
                std::hash::Hash::hash(&3u32, state);
                left
            }
            Term__view::TVar(f) => {
                std::hash::Hash::hash(&4u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Term__view::THole => {
                std::hash::Hash::hash(&5u32, state);
                left
            }
            Term__view::TGet(f) => {
                std::hash::Hash::hash(&6u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                std::hash::Hash::hash(&f.1, state);
                left
            }
            Term__view::TCall(f) => {
                std::hash::Hash::hash(&7u32, state);
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::shared_items::stream_items(
                    f.1.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TBi(f) => {
                std::hash::Hash::hash(&8u32, state);
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::shared_items::stream_items(
                    f.1.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TOp(f) => {
                std::hash::Hash::hash(&9u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                left
            }
            Term__view::TNeg(f) => {
                std::hash::Hash::hash(&10u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Term__view::TCtor(f) => {
                std::hash::Hash::hash(&11u32, state);
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::shared_items::stream_items(
                    f.1.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TMatch(f) => {
                std::hash::Hash::hash(&12u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::shared_items::stream_items(
                    f.1.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TParts(f) => {
                std::hash::Hash::hash(&13u32, state);
                let left = aver_rt::shared_items::stream_items(
                    f.0.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TList(f) => {
                std::hash::Hash::hash(&14u32, state);
                let left = aver_rt::shared_items::stream_items(
                    f.0.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TTuple(f) => {
                std::hash::Hash::hash(&15u32, state);
                let left = aver_rt::shared_items::stream_items(
                    f.0.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TRec(f) => {
                std::hash::Hash::hash(&16u32, state);
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::shared_items::stream_items(
                    f.1.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Term__view::TUpd(f) => {
                std::hash::Hash::hash(&17u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::shared_items::stream_items(
                    f.2.iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self.view() {
            Term__view::TInt(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&0u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Term__view::TBool(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&1u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Term__view::TStr(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&2u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Term__view::TUnit => aver_rt::shared_hash::tag_hash(3),
            Term__view::TVar(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&4u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Term__view::THole => aver_rt::shared_hash::tag_hash(5),
            Term__view::TGet(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&6u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hash::hash(&f.1, h);
            }),
            Term__view::TCall(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&7u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.1, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TBi(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&8u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.1, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TOp(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&9u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
            }),
            Term__view::TNeg(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&10u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Term__view::TCtor(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&11u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.1, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TMatch(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&12u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.1, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TParts(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&13u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.0, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TList(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&14u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.0, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TTuple(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&15u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.0, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TRec(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&16u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.1, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
            Term__view::TUpd(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&17u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&f.2, walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
            }),
        }
    }
}

impl Eq for Term {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TInt(pub aver_rt::AverInt);

impl PartialEq for Term__TInt {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Term__TInt {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TBool(pub bool);

impl PartialEq for Term__TBool {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Term__TBool {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TStr(pub AverStr);

impl PartialEq for Term__TStr {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Term__TStr {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TVar(pub AverStr);

impl PartialEq for Term__TVar {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Term__TVar {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TGet(pub Term, pub AverStr);

impl PartialEq for Term__TGet {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.1 == other.1 && self.0 == other.0
    }
}

impl aver_rt::WalkEq for Term__TGet {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.1 == other.1 && self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Term__TGet {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TCall(pub AverStr, pub aver_rt::AverList<Term>);

impl PartialEq for Term__TCall {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Term__TCall {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && aver_rt::shared_items::eq_list(&self.1, &other.1, walk, |x, y, walk| {
                (*x).walk_eq(&(*y), walk)
            })
    }
}

impl aver_rt::thin::HandOver for Term__TCall {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TBi(pub AverStr, pub aver_rt::AverList<Term>);

impl PartialEq for Term__TBi {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Term__TBi {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && aver_rt::shared_items::eq_list(&self.1, &other.1, walk, |x, y, walk| {
                (*x).walk_eq(&(*y), walk)
            })
    }
}

impl aver_rt::thin::HandOver for Term__TBi {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TOp(pub AverStr, pub Term, pub Term);

impl PartialEq for Term__TOp {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1 && self.2 == other.2
    }
}

impl aver_rt::WalkEq for Term__TOp {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0 && self.1.walk_eq(&other.1, walk) && self.2.walk_eq(&other.2, walk)
    }
}

impl aver_rt::thin::HandOver for Term__TOp {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TNeg(pub Term);

impl PartialEq for Term__TNeg {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Term__TNeg {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Term__TNeg {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TCtor(pub AverStr, pub aver_rt::AverList<Term>);

impl PartialEq for Term__TCtor {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Term__TCtor {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && aver_rt::shared_items::eq_list(&self.1, &other.1, walk, |x, y, walk| {
                (*x).walk_eq(&(*y), walk)
            })
    }
}

impl aver_rt::thin::HandOver for Term__TCtor {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TMatch(pub Term, pub aver_rt::AverList<Arm>);

impl PartialEq for Term__TMatch {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Term__TMatch {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
            && aver_rt::shared_items::eq_list(&self.1, &other.1, walk, |x, y, walk| {
                (*x).walk_eq(&(*y), walk)
            })
    }
}

impl aver_rt::thin::HandOver for Term__TMatch {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TParts(pub aver_rt::AverList<Term>);

impl PartialEq for Term__TParts {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Term__TParts {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        aver_rt::shared_items::eq_list(&self.0, &other.0, walk, |x, y, walk| {
            (*x).walk_eq(&(*y), walk)
        })
    }
}

impl aver_rt::thin::HandOver for Term__TParts {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TList(pub aver_rt::AverList<Term>);

impl PartialEq for Term__TList {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Term__TList {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        aver_rt::shared_items::eq_list(&self.0, &other.0, walk, |x, y, walk| {
            (*x).walk_eq(&(*y), walk)
        })
    }
}

impl aver_rt::thin::HandOver for Term__TList {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TTuple(pub aver_rt::AverList<Term>);

impl PartialEq for Term__TTuple {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Term__TTuple {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        aver_rt::shared_items::eq_list(&self.0, &other.0, walk, |x, y, walk| {
            (*x).walk_eq(&(*y), walk)
        })
    }
}

impl aver_rt::thin::HandOver for Term__TTuple {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TRec(pub AverStr, pub aver_rt::AverList<Field>);

impl PartialEq for Term__TRec {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Term__TRec {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && aver_rt::shared_items::eq_list(&self.1, &other.1, walk, |x, y, walk| {
                (*x).walk_eq(&(*y), walk)
            })
    }
}

impl aver_rt::thin::HandOver for Term__TRec {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TUpd(pub AverStr, pub Term, pub aver_rt::AverList<Field>);

impl PartialEq for Term__TUpd {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1 && self.2 == other.2
    }
}

impl aver_rt::WalkEq for Term__TUpd {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1.walk_eq(&other.1, walk)
            && aver_rt::shared_items::eq_list(&self.2, &other.2, walk, |x, y, walk| {
                (*x).walk_eq(&(*y), walk)
            })
    }
}

impl aver_rt::thin::HandOver for Term__TUpd {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

impl aver_rt::WalkEq for Term {
    #[inline(always)]
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        Term::ptr_eq(self, other) || self.aver_walk_eq_nodes(other, walk)
    }
}

impl Term {
    #[inline(never)]
    fn aver_walk_eq_nodes<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        use ::aver_rt::WalkEq as _;
        match Term::view_pair(self, other) {
            Some(Term__pair::TInt(a0, b0)) => a0 == b0,
            Some(Term__pair::TBool(a0, b0)) => a0 == b0,
            Some(Term__pair::TStr(a0, b0)) => a0 == b0,
            Some(Term__pair::TUnit) => true,
            Some(Term__pair::TVar(a0, b0)) => a0 == b0,
            Some(Term__pair::THole) => true,
            Some(Term__pair::TGet(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TCall(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TBi(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TOp(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TNeg(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TCtor(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TMatch(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TParts(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TList(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TTuple(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TRec(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Term__pair::TUpd(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            None => false,
        }
    }
}

impl PartialEq for Term {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::walk_eq(self, other)
    }
}

impl Term {
    fn aver_key_rank(&self) -> usize {
        match self.view() {
            Term__view::TBi(..) => 0,
            Term__view::TBool(..) => 1,
            Term__view::TCall(..) => 2,
            Term__view::TCtor(..) => 3,
            Term__view::TGet(..) => 4,
            Term__view::THole => 5,
            Term__view::TInt(..) => 6,
            Term__view::TList(..) => 7,
            Term__view::TMatch(..) => 8,
            Term__view::TNeg(..) => 9,
            Term__view::TOp(..) => 10,
            Term__view::TParts(..) => 11,
            Term__view::TRec(..) => 12,
            Term__view::TStr(..) => 13,
            Term__view::TTuple(..) => 14,
            Term__view::TUnit => 15,
            Term__view::TUpd(..) => 16,
            Term__view::TVar(..) => 17,
        }
    }
}

impl PartialOrd for Term {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Term {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match Term::view_pair(self, other) {
            Some(Term__pair::TBi(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Term__pair::TBool(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TCall(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Term__pair::TCtor(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Term__pair::TGet(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Term__pair::TInt(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TList(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TMatch(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Term__pair::TNeg(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TOp(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            Some(Term__pair::TParts(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TRec(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            Some(Term__pair::TStr(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TTuple(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Term__pair::TUpd(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            Some(Term__pair::TVar(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Term {
    fn aver_display(&self) -> String {
        match self.view() {
            Term__view::TInt(f) => format!("TInt({})", (&f.0).aver_display_inner()),
            Term__view::TBool(f) => format!("TBool({})", (&f.0).aver_display_inner()),
            Term__view::TStr(f) => format!("TStr({})", (&f.0).aver_display_inner()),
            Term__view::TUnit => "TUnit".to_string(),
            Term__view::TVar(f) => format!("TVar({})", (&f.0).aver_display_inner()),
            Term__view::THole => "THole".to_string(),
            Term__view::TGet(f) => format!(
                "TGet({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term__view::TCall(f) => format!(
                "TCall({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term__view::TBi(f) => format!(
                "TBi({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term__view::TOp(f) => format!(
                "TOp({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Term__view::TNeg(f) => format!("TNeg({})", (&f.0).aver_display_inner()),
            Term__view::TCtor(f) => format!(
                "TCtor({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term__view::TMatch(f) => format!(
                "TMatch({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term__view::TParts(f) => format!("TParts({})", (&f.0).aver_display_inner()),
            Term__view::TList(f) => format!("TList({})", (&f.0).aver_display_inner()),
            Term__view::TTuple(f) => format!("TTuple({})", (&f.0).aver_display_inner()),
            Term__view::TRec(f) => format!(
                "TRec({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term__view::TUpd(f) => format!(
                "TUpd({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, Eq)]
pub struct Arm {
    pub pattern: Pat,
    pub body: Term,
}

impl Arm {
    fn aver_fast_eq(&self, other: &Self) -> bool {
        self.pattern.aver_fast_eq(&other.pattern) && self.body == other.body
    }
}

impl PartialEq for Arm {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::shared_items::top(self, other, Self::aver_fast_eq)
    }
}

impl aver_rt::WalkEq for Arm {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.pattern.walk_eq(&other.pattern, walk) && self.body.walk_eq(&other.body, walk)
    }
}

impl std::hash::Hash for Arm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Arm {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        if left == 0 {
            return 0;
        }
        let left = left - 1;
        let left = aver_rt::WalkHash::stream(&self.pattern, state, left);
        let left = aver_rt::WalkHash::stream(&self.body, state, left);
        left
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        let h = &mut aver_rt::shared_hash::node_hasher();
        std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&self.pattern, walk));
        std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&self.body, walk));
        std::hash::Hasher::finish(h)
    }
}

impl PartialOrd for Arm {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Arm {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.body.cmp(&other.body))
            .then_with(|| self.pattern.cmp(&other.pattern))
    }
}

impl aver_rt::AverDisplay for Arm {
    fn aver_display(&self) -> String {
        format!(
            "Arm({})",
            vec![
                format!("pattern: {}", self.pattern.aver_display_inner()),
                format!("body: {}", self.body.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, Eq)]
pub struct Field {
    pub name: AverStr,
    pub value: Term,
}

impl Field {
    fn aver_fast_eq(&self, other: &Self) -> bool {
        self.name == other.name && self.value == other.value
    }
}

impl PartialEq for Field {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::shared_items::top(self, other, Self::aver_fast_eq)
    }
}

impl aver_rt::WalkEq for Field {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.name == other.name && self.value.walk_eq(&other.value, walk)
    }
}

impl std::hash::Hash for Field {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Field {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        std::hash::Hash::hash(&self.name, state);
        let left = aver_rt::WalkHash::stream(&self.value, state, left);
        left
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        let h = &mut aver_rt::shared_hash::node_hasher();
        std::hash::Hash::hash(&self.name, h);
        std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&self.value, walk));
        std::hash::Hasher::finish(h)
    }
}

impl PartialOrd for Field {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Field {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.value.cmp(&other.value))
    }
}

impl aver_rt::AverDisplay for Field {
    fn aver_display(&self) -> String {
        format!(
            "Field({})",
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

#[derive(Clone, Debug, Eq)]
pub enum Pat {
    PWild,
    PVar(AverStr),
    PLit(Term),
    PNil,
    PCons(AverStr, AverStr),
    PTuple(aver_rt::AverList<Pat>),
    PCtor(AverStr, aver_rt::AverList<AverStr>),
}

impl std::hash::Hash for Pat {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Pat {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self {
            Pat::PWild => {
                std::hash::Hash::hash(&0u32, state);
                left
            }
            Pat::PVar(f0) => {
                std::hash::Hash::hash(&1u32, state);
                std::hash::Hash::hash(&(*f0), state);
                left
            }
            Pat::PLit(f0) => {
                std::hash::Hash::hash(&2u32, state);
                let left = aver_rt::WalkHash::stream(&(*f0), state, left);
                left
            }
            Pat::PNil => {
                std::hash::Hash::hash(&3u32, state);
                left
            }
            Pat::PCons(f0, f1) => {
                std::hash::Hash::hash(&4u32, state);
                std::hash::Hash::hash(&(*f0), state);
                std::hash::Hash::hash(&(*f1), state);
                left
            }
            Pat::PTuple(f0) => {
                std::hash::Hash::hash(&5u32, state);
                let left = aver_rt::shared_items::stream_items(
                    (*f0).iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Pat::PCtor(f0, f1) => {
                std::hash::Hash::hash(&6u32, state);
                std::hash::Hash::hash(&(*f0), state);
                std::hash::Hash::hash(&(*f1), state);
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self {
            Pat::PWild => aver_rt::shared_hash::tag_hash(0),
            Pat::PVar(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&1u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hasher::finish(h)
            }
            Pat::PLit(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&2u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&(*f0), walk));
                std::hash::Hasher::finish(h)
            }
            Pat::PNil => aver_rt::shared_hash::tag_hash(3),
            Pat::PCons(f0, f1) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&4u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hash::hash(&(*f1), h);
                std::hash::Hasher::finish(h)
            }
            Pat::PTuple(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&5u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&(*f0), walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
                std::hash::Hasher::finish(h)
            }
            Pat::PCtor(f0, f1) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&6u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hash::hash(&(*f1), h);
                std::hash::Hasher::finish(h)
            }
        }
    }
}

impl aver_rt::WalkEq for Pat {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        match (self, other) {
            (Pat::PWild, Pat::PWild) => true,
            (Pat::PVar(a0), Pat::PVar(b0)) => a0 == b0,
            (Pat::PLit(a0), Pat::PLit(b0)) => (*a0).walk_eq(&(*b0), walk),
            (Pat::PNil, Pat::PNil) => true,
            (Pat::PCons(a0, a1), Pat::PCons(b0, b1)) => a0 == b0 && a1 == b1,
            (Pat::PTuple(a0), Pat::PTuple(b0)) => {
                aver_rt::shared_items::eq_list(&(*a0), &(*b0), walk, |x, y, walk| {
                    (*x).walk_eq(&(*y), walk)
                })
            }
            (Pat::PCtor(a0, a1), Pat::PCtor(b0, b1)) => a0 == b0 && a1 == b1,
            _ => false,
        }
    }
}

impl Pat {
    fn aver_fast_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Pat::PWild, Pat::PWild) => true,
            (Pat::PVar(a0), Pat::PVar(b0)) => (*a0) == (*b0),
            (Pat::PLit(a0), Pat::PLit(b0)) => (*a0) == (*b0),
            (Pat::PNil, Pat::PNil) => true,
            (Pat::PCons(a0, a1), Pat::PCons(b0, b1)) => (*a0) == (*b0) && (*a1) == (*b1),
            (Pat::PTuple(a0), Pat::PTuple(b0)) => {
                aver_rt::shared_items::fast_list(&(*a0), &(*b0), |x, y| (*x).aver_fast_eq(&(*y)))
            }
            (Pat::PCtor(a0, a1), Pat::PCtor(b0, b1)) => (*a0) == (*b0) && (*a1) == (*b1),
            _ => false,
        }
    }
}

impl PartialEq for Pat {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::shared_items::top(self, other, Self::aver_fast_eq)
    }
}

impl Pat {
    fn aver_key_rank(&self) -> usize {
        match self {
            Pat::PCons(..) => 0,
            Pat::PCtor(..) => 1,
            Pat::PLit(..) => 2,
            Pat::PNil => 3,
            Pat::PTuple(..) => 4,
            Pat::PVar(..) => 5,
            Pat::PWild => 6,
        }
    }
}

impl PartialOrd for Pat {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pat {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Pat::PCons(a0, a1), Pat::PCons(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Pat::PCtor(a0, a1), Pat::PCtor(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Pat::PLit(a0), Pat::PLit(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Pat::PTuple(a0), Pat::PTuple(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Pat::PVar(a0), Pat::PVar(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Pat {
    fn aver_display(&self) -> String {
        match self {
            Pat::PWild => "PWild".to_string(),
            Pat::PVar(f0) => format!("PVar({})", f0.aver_display_inner()),
            Pat::PLit(f0) => format!("PLit({})", f0.aver_display_inner()),
            Pat::PNil => "PNil".to_string(),
            Pat::PCons(f0, f1) => format!(
                "PCons({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Pat::PTuple(f0) => format!("PTuple({})", f0.aver_display_inner()),
            Pat::PCtor(f0, f1) => format!(
                "PCtor({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Binding {
    pub name: AverStr,
    pub value: Term,
}

impl PartialOrd for Binding {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Binding {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.value.cmp(&other.value))
    }
}

impl aver_rt::AverDisplay for Binding {
    fn aver_display(&self) -> String {
        format!(
            "Binding({})",
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
pub struct Eqn {
    pub lhs: Term,
    pub rhs: Term,
}

impl PartialOrd for Eqn {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Eqn {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.lhs.cmp(&other.lhs))
            .then_with(|| self.rhs.cmp(&other.rhs))
    }
}

impl aver_rt::AverDisplay for Eqn {
    fn aver_display(&self) -> String {
        format!(
            "Eqn({})",
            vec![
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

#[derive(Clone, Debug, Eq)]
pub enum Fin {
    FBool,
    FSum(aver_rt::AverList<AverStr>),
    FRec(AverStr, aver_rt::AverList<FinField>),
    FTuple(aver_rt::AverList<Fin>),
}

impl std::hash::Hash for Fin {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Fin {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self {
            Fin::FBool => {
                std::hash::Hash::hash(&0u32, state);
                left
            }
            Fin::FSum(f0) => {
                std::hash::Hash::hash(&1u32, state);
                std::hash::Hash::hash(&(*f0), state);
                left
            }
            Fin::FRec(f0, f1) => {
                std::hash::Hash::hash(&2u32, state);
                std::hash::Hash::hash(&(*f0), state);
                let left = aver_rt::shared_items::stream_items(
                    (*f1).iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
            Fin::FTuple(f0) => {
                std::hash::Hash::hash(&3u32, state);
                let left = aver_rt::shared_items::stream_items(
                    (*f0).iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self {
            Fin::FBool => aver_rt::shared_hash::tag_hash(0),
            Fin::FSum(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&1u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hasher::finish(h)
            }
            Fin::FRec(f0, f1) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&2u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&(*f1), walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
                std::hash::Hasher::finish(h)
            }
            Fin::FTuple(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&3u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&(*f0), walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
                std::hash::Hasher::finish(h)
            }
        }
    }
}

impl aver_rt::WalkEq for Fin {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        match (self, other) {
            (Fin::FBool, Fin::FBool) => true,
            (Fin::FSum(a0), Fin::FSum(b0)) => a0 == b0,
            (Fin::FRec(a0, a1), Fin::FRec(b0, b1)) => {
                (*a0) == (*b0)
                    && aver_rt::shared_items::eq_list(&(*a1), &(*b1), walk, |x, y, walk| {
                        (*x).walk_eq(&(*y), walk)
                    })
            }
            (Fin::FTuple(a0), Fin::FTuple(b0)) => {
                aver_rt::shared_items::eq_list(&(*a0), &(*b0), walk, |x, y, walk| {
                    (*x).walk_eq(&(*y), walk)
                })
            }
            _ => false,
        }
    }
}

impl Fin {
    fn aver_fast_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Fin::FBool, Fin::FBool) => true,
            (Fin::FSum(a0), Fin::FSum(b0)) => (*a0) == (*b0),
            (Fin::FRec(a0, a1), Fin::FRec(b0, b1)) => {
                (*a0) == (*b0)
                    && aver_rt::shared_items::fast_list(&(*a1), &(*b1), |x, y| {
                        (*x).aver_fast_eq(&(*y))
                    })
            }
            (Fin::FTuple(a0), Fin::FTuple(b0)) => {
                aver_rt::shared_items::fast_list(&(*a0), &(*b0), |x, y| (*x).aver_fast_eq(&(*y)))
            }
            _ => false,
        }
    }
}

impl PartialEq for Fin {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::shared_items::top(self, other, Self::aver_fast_eq)
    }
}

impl Fin {
    fn aver_key_rank(&self) -> usize {
        match self {
            Fin::FBool => 0,
            Fin::FRec(..) => 1,
            Fin::FSum(..) => 2,
            Fin::FTuple(..) => 3,
        }
    }
}

impl PartialOrd for Fin {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fin {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Fin::FRec(a0, a1), Fin::FRec(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Fin::FSum(a0), Fin::FSum(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Fin::FTuple(a0), Fin::FTuple(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Fin {
    fn aver_display(&self) -> String {
        match self {
            Fin::FBool => "FBool".to_string(),
            Fin::FSum(f0) => format!("FSum({})", f0.aver_display_inner()),
            Fin::FRec(f0, f1) => format!(
                "FRec({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Fin::FTuple(f0) => format!("FTuple({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, Eq)]
pub struct FinField {
    pub name: AverStr,
    pub fin: Fin,
}

impl FinField {
    fn aver_fast_eq(&self, other: &Self) -> bool {
        self.name == other.name && self.fin.aver_fast_eq(&other.fin)
    }
}

impl PartialEq for FinField {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::shared_items::top(self, other, Self::aver_fast_eq)
    }
}

impl aver_rt::WalkEq for FinField {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.name == other.name && self.fin.walk_eq(&other.fin, walk)
    }
}

impl std::hash::Hash for FinField {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for FinField {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        std::hash::Hash::hash(&self.name, state);
        let left = aver_rt::WalkHash::stream(&self.fin, state, left);
        left
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        let h = &mut aver_rt::shared_hash::node_hasher();
        std::hash::Hash::hash(&self.name, h);
        std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&self.fin, walk));
        std::hash::Hasher::finish(h)
    }
}

impl PartialOrd for FinField {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FinField {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.fin.cmp(&other.fin))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for FinField {
    fn aver_display(&self) -> String {
        format!(
            "FinField({})",
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

/// Whether a term is the hole.
pub fn isHole(t @ _: &Term) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::THole => true,
            _ => false,
        }
    }
}

/// One substitution entry.
pub fn bind(name @ _: AverStr, mut value @ _: Term) -> Binding {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::term::Binding {
        name: name,
        value: value,
    }
}

/// A variable with its type.
pub fn typed(name @ _: AverStr, mut ty @ _: Ty) -> TypedVar {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::term::TypedVar { name: name, ty: ty }
}
