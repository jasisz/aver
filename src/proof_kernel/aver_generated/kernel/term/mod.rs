#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, Eq, Hash)]
pub enum Ty {
    TyInt,
    TyBool,
    TyStr,
    TyFloat,
    TyUnit,
    TyList(std::sync::Arc<Ty__TyList>),
    TyOption(std::sync::Arc<Ty__TyOption>),
    TyResult(std::sync::Arc<Ty__TyResult>),
    TyMap(std::sync::Arc<Ty__TyMap>),
    TyVector(std::sync::Arc<Ty__TyVector>),
    TyTuple(std::sync::Arc<Ty__TyTuple>),
    TyNamed(std::sync::Arc<Ty__TyNamed>),
    TyAny,
    TyVar(std::sync::Arc<Ty__TyVar>),
    TyRigid(std::sync::Arc<Ty__TyRigid>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyList(pub Ty);

impl aver_rt::WalkEq for Ty__TyList {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyOption(pub Ty);

impl aver_rt::WalkEq for Ty__TyOption {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyResult(pub Ty, pub Ty);

impl aver_rt::WalkEq for Ty__TyResult {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyMap(pub Ty, pub Ty);

impl aver_rt::WalkEq for Ty__TyMap {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyVector(pub Ty);

impl aver_rt::WalkEq for Ty__TyVector {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyTuple(pub aver_rt::AverList<Ty>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyNamed(pub AverStr);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyVar(pub AverStr, pub bool);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty__TyRigid(pub AverStr, pub bool);

impl aver_rt::WalkEq for Ty {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
            && match (self, other) {
                (Ty::TyInt, Ty::TyInt) => true,
                (Ty::TyBool, Ty::TyBool) => true,
                (Ty::TyStr, Ty::TyStr) => true,
                (Ty::TyFloat, Ty::TyFloat) => true,
                (Ty::TyUnit, Ty::TyUnit) => true,
                (Ty::TyList(a0), Ty::TyList(b0)) => walk.shared(a0, b0, true),
                (Ty::TyOption(a0), Ty::TyOption(b0)) => walk.shared(a0, b0, true),
                (Ty::TyResult(a0), Ty::TyResult(b0)) => walk.shared(a0, b0, true),
                (Ty::TyMap(a0), Ty::TyMap(b0)) => walk.shared(a0, b0, true),
                (Ty::TyVector(a0), Ty::TyVector(b0)) => walk.shared(a0, b0, true),
                (Ty::TyTuple(a0), Ty::TyTuple(b0)) => a0 == b0,
                (Ty::TyNamed(a0), Ty::TyNamed(b0)) => a0 == b0,
                (Ty::TyAny, Ty::TyAny) => true,
                (Ty::TyVar(a0), Ty::TyVar(b0)) => a0 == b0,
                (Ty::TyRigid(a0), Ty::TyRigid(b0)) => a0 == b0,
                _ => false,
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
        match self {
            Ty::TyAny => 0,
            Ty::TyBool => 1,
            Ty::TyFloat => 2,
            Ty::TyInt => 3,
            Ty::TyList(..) => 4,
            Ty::TyMap(..) => 5,
            Ty::TyNamed(..) => 6,
            Ty::TyOption(..) => 7,
            Ty::TyResult(..) => 8,
            Ty::TyRigid(..) => 9,
            Ty::TyStr => 10,
            Ty::TyTuple(..) => 11,
            Ty::TyUnit => 12,
            Ty::TyVar(..) => 13,
            Ty::TyVector(..) => 14,
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
        match (self, other) {
            (Ty::TyList(a), Ty::TyList(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Ty::TyMap(a), Ty::TyMap(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Ty::TyNamed(a), Ty::TyNamed(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Ty::TyOption(a), Ty::TyOption(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Ty::TyResult(a), Ty::TyResult(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Ty::TyRigid(a), Ty::TyRigid(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Ty::TyTuple(a), Ty::TyTuple(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Ty::TyVar(a), Ty::TyVar(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Ty::TyVector(a), Ty::TyVector(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Ty {
    fn aver_display(&self) -> String {
        match self {
            Ty::TyInt => "TyInt".to_string(),
            Ty::TyBool => "TyBool".to_string(),
            Ty::TyStr => "TyStr".to_string(),
            Ty::TyFloat => "TyFloat".to_string(),
            Ty::TyUnit => "TyUnit".to_string(),
            Ty::TyList(f) => format!("TyList({})", (&f.0).aver_display_inner()),
            Ty::TyOption(f) => format!("TyOption({})", (&f.0).aver_display_inner()),
            Ty::TyResult(f) => format!(
                "TyResult({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Ty::TyMap(f) => format!(
                "TyMap({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Ty::TyVector(f) => format!("TyVector({})", (&f.0).aver_display_inner()),
            Ty::TyTuple(f) => format!("TyTuple({})", (&f.0).aver_display_inner()),
            Ty::TyNamed(f) => format!("TyNamed({})", (&f.0).aver_display_inner()),
            Ty::TyAny => "TyAny".to_string(),
            Ty::TyVar(f) => format!(
                "TyVar({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Ty::TyRigid(f) => format!(
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

#[derive(Clone, Debug, Eq, Hash)]
pub enum Term {
    TInt(std::sync::Arc<Term__TInt>),
    TBool(std::sync::Arc<Term__TBool>),
    TStr(std::sync::Arc<Term__TStr>),
    TUnit,
    TVar(std::sync::Arc<Term__TVar>),
    THole,
    TGet(std::sync::Arc<Term__TGet>),
    TCall(std::sync::Arc<Term__TCall>),
    TBi(std::sync::Arc<Term__TBi>),
    TOp(std::sync::Arc<Term__TOp>),
    TNeg(std::sync::Arc<Term__TNeg>),
    TCtor(std::sync::Arc<Term__TCtor>),
    TMatch(std::sync::Arc<Term__TMatch>),
    TParts(std::sync::Arc<Term__TParts>),
    TList(std::sync::Arc<Term__TList>),
    TTuple(std::sync::Arc<Term__TTuple>),
    TRec(std::sync::Arc<Term__TRec>),
    TUpd(std::sync::Arc<Term__TUpd>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TInt(pub aver_rt::AverInt);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TBool(pub bool);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TStr(pub AverStr);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TVar(pub AverStr);

#[derive(Clone, Debug, Eq, Hash)]
pub struct Term__TGet(pub Term, pub AverStr);

impl PartialEq for Term__TGet {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && self.0 == other.0
    }
}

impl aver_rt::WalkEq for Term__TGet {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.1 == other.1 && self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TCall(pub AverStr, pub aver_rt::AverList<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TBi(pub AverStr, pub aver_rt::AverList<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TOp(pub AverStr, pub Term, pub Term);

impl aver_rt::WalkEq for Term__TOp {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0 == other.0 && self.1.walk_eq(&other.1, walk) && self.2.walk_eq(&other.2, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TNeg(pub Term);

impl aver_rt::WalkEq for Term__TNeg {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TCtor(pub AverStr, pub aver_rt::AverList<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TMatch(pub Term, pub aver_rt::AverList<Arm>);

impl aver_rt::WalkEq for Term__TMatch {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1 == other.1
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TParts(pub aver_rt::AverList<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TList(pub aver_rt::AverList<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TTuple(pub aver_rt::AverList<Term>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TRec(pub AverStr, pub aver_rt::AverList<Field>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Term__TUpd(pub AverStr, pub Term, pub aver_rt::AverList<Field>);

impl aver_rt::WalkEq for Term__TUpd {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0 == other.0 && self.1.walk_eq(&other.1, walk) && self.2 == other.2
    }
}

impl aver_rt::WalkEq for Term {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
            && match (self, other) {
                (Term::TInt(a0), Term::TInt(b0)) => a0 == b0,
                (Term::TBool(a0), Term::TBool(b0)) => a0 == b0,
                (Term::TStr(a0), Term::TStr(b0)) => a0 == b0,
                (Term::TUnit, Term::TUnit) => true,
                (Term::TVar(a0), Term::TVar(b0)) => a0 == b0,
                (Term::THole, Term::THole) => true,
                (Term::TGet(a0), Term::TGet(b0)) => walk.shared(a0, b0, true),
                (Term::TCall(a0), Term::TCall(b0)) => a0 == b0,
                (Term::TBi(a0), Term::TBi(b0)) => a0 == b0,
                (Term::TOp(a0), Term::TOp(b0)) => walk.shared(a0, b0, true),
                (Term::TNeg(a0), Term::TNeg(b0)) => walk.shared(a0, b0, true),
                (Term::TCtor(a0), Term::TCtor(b0)) => a0 == b0,
                (Term::TMatch(a0), Term::TMatch(b0)) => walk.shared(a0, b0, true),
                (Term::TParts(a0), Term::TParts(b0)) => a0 == b0,
                (Term::TList(a0), Term::TList(b0)) => a0 == b0,
                (Term::TTuple(a0), Term::TTuple(b0)) => a0 == b0,
                (Term::TRec(a0), Term::TRec(b0)) => a0 == b0,
                (Term::TUpd(a0), Term::TUpd(b0)) => walk.shared(a0, b0, true),
                _ => false,
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
        match self {
            Term::TBi(..) => 0,
            Term::TBool(..) => 1,
            Term::TCall(..) => 2,
            Term::TCtor(..) => 3,
            Term::TGet(..) => 4,
            Term::THole => 5,
            Term::TInt(..) => 6,
            Term::TList(..) => 7,
            Term::TMatch(..) => 8,
            Term::TNeg(..) => 9,
            Term::TOp(..) => 10,
            Term::TParts(..) => 11,
            Term::TRec(..) => 12,
            Term::TStr(..) => 13,
            Term::TTuple(..) => 14,
            Term::TUnit => 15,
            Term::TUpd(..) => 16,
            Term::TVar(..) => 17,
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
        match (self, other) {
            (Term::TBi(a), Term::TBi(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Term::TBool(a), Term::TBool(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TCall(a), Term::TCall(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Term::TCtor(a), Term::TCtor(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Term::TGet(a), Term::TGet(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Term::TInt(a), Term::TInt(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TList(a), Term::TList(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TMatch(a), Term::TMatch(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Term::TNeg(a), Term::TNeg(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TOp(a), Term::TOp(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            (Term::TParts(a), Term::TParts(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TRec(a), Term::TRec(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1))),
            (Term::TStr(a), Term::TStr(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TTuple(a), Term::TTuple(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            (Term::TUpd(a), Term::TUpd(b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            (Term::TVar(a), Term::TVar(b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Term {
    fn aver_display(&self) -> String {
        match self {
            Term::TInt(f) => format!("TInt({})", (&f.0).aver_display_inner()),
            Term::TBool(f) => format!("TBool({})", (&f.0).aver_display_inner()),
            Term::TStr(f) => format!("TStr({})", (&f.0).aver_display_inner()),
            Term::TUnit => "TUnit".to_string(),
            Term::TVar(f) => format!("TVar({})", (&f.0).aver_display_inner()),
            Term::THole => "THole".to_string(),
            Term::TGet(f) => format!(
                "TGet({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term::TCall(f) => format!(
                "TCall({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term::TBi(f) => format!(
                "TBi({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term::TOp(f) => format!(
                "TOp({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Term::TNeg(f) => format!("TNeg({})", (&f.0).aver_display_inner()),
            Term::TCtor(f) => format!(
                "TCtor({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term::TMatch(f) => format!(
                "TMatch({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term::TParts(f) => format!("TParts({})", (&f.0).aver_display_inner()),
            Term::TList(f) => format!("TList({})", (&f.0).aver_display_inner()),
            Term::TTuple(f) => format!("TTuple({})", (&f.0).aver_display_inner()),
            Term::TRec(f) => format!(
                "TRec({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Term::TUpd(f) => format!(
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Arm {
    pub pattern: Pat,
    pub body: Term,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: AverStr,
    pub value: Term,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pat {
    PWild,
    PVar(AverStr),
    PLit(Term),
    PNil,
    PCons(AverStr, AverStr),
    PTuple(aver_rt::AverList<Pat>),
    PCtor(AverStr, aver_rt::AverList<AverStr>),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Fin {
    FBool,
    FSum(aver_rt::AverList<AverStr>),
    FRec(AverStr, aver_rt::AverList<FinField>),
    FTuple(aver_rt::AverList<Fin>),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FinField {
    pub name: AverStr,
    pub fin: Fin,
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
    crate::proof_kernel::cancel_checkpoint();
    match t {
        crate::proof_kernel::aver_generated::kernel::term::Term::THole => true,
        _ => false,
    }
}

/// One substitution entry.
pub fn bind(name @ _: AverStr, mut value @ _: Term) -> Binding {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Binding {
        name: name,
        value: value,
    }
}

/// A variable with its type.
pub fn typed(name @ _: AverStr, mut ty @ _: Ty) -> TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::TypedVar { name: name, ty: ty }
}
