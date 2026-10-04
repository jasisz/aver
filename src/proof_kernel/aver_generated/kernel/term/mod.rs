#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    TInt(aver_rt::AverInt),
    TBool(bool),
    TStr(AverStr),
    TUnit,
    TVar(AverStr),
    THole,
    TGet(std::sync::Arc<Term>, AverStr),
    TCall(AverStr, aver_rt::AverList<Term>),
    TBi(AverStr, aver_rt::AverList<Term>),
    TOp(AverStr, std::sync::Arc<Term>, std::sync::Arc<Term>),
    TNeg(std::sync::Arc<Term>),
    TCtor(AverStr, aver_rt::AverList<Term>),
    TMatch(std::sync::Arc<Term>, aver_rt::AverList<Arm>),
    TParts(aver_rt::AverList<Term>),
    TList(aver_rt::AverList<Term>),
    TTuple(aver_rt::AverList<Term>),
    TRec(AverStr, aver_rt::AverList<Field>),
    TUpd(AverStr, std::sync::Arc<Term>, aver_rt::AverList<Field>),
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
            (Term::TBi(a0, a1), Term::TBi(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Term::TBool(a0), Term::TBool(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Term::TCall(a0, a1), Term::TCall(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Term::TCtor(a0, a1), Term::TCtor(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Term::TGet(a0, a1), Term::TGet(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Term::TInt(a0), Term::TInt(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Term::TList(a0), Term::TList(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Term::TMatch(a0, a1), Term::TMatch(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Term::TNeg(a0), Term::TNeg(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Term::TOp(a0, a1, a2), Term::TOp(b0, b1, b2)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1))
                .then_with(|| a2.cmp(b2)),
            (Term::TParts(a0), Term::TParts(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Term::TRec(a0, a1), Term::TRec(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Term::TStr(a0), Term::TStr(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Term::TTuple(a0), Term::TTuple(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Term::TUpd(a0, a1, a2), Term::TUpd(b0, b1, b2)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1))
                .then_with(|| a2.cmp(b2)),
            (Term::TVar(a0), Term::TVar(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Term {
    fn aver_display(&self) -> String {
        match self {
            Term::TInt(f0) => format!("TInt({})", f0.aver_display_inner()),
            Term::TBool(f0) => format!("TBool({})", f0.aver_display_inner()),
            Term::TStr(f0) => format!("TStr({})", f0.aver_display_inner()),
            Term::TUnit => "TUnit".to_string(),
            Term::TVar(f0) => format!("TVar({})", f0.aver_display_inner()),
            Term::THole => "THole".to_string(),
            Term::TGet(f0, f1) => format!(
                "TGet({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Term::TCall(f0, f1) => format!(
                "TCall({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Term::TBi(f0, f1) => format!(
                "TBi({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Term::TOp(f0, f1, f2) => format!(
                "TOp({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Term::TNeg(f0) => format!("TNeg({})", f0.aver_display_inner()),
            Term::TCtor(f0, f1) => format!(
                "TCtor({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Term::TMatch(f0, f1) => format!(
                "TMatch({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Term::TParts(f0) => format!("TParts({})", f0.aver_display_inner()),
            Term::TList(f0) => format!("TList({})", f0.aver_display_inner()),
            Term::TTuple(f0) => format!("TTuple({})", f0.aver_display_inner()),
            Term::TRec(f0, f1) => format!(
                "TRec({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Term::TUpd(f0, f1, f2) => format!(
                "TUpd({})",
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
pub fn bind(name @ _: AverStr, value @ _: &Term) -> Binding {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Binding {
        name: name,
        value: value.clone(),
    }
}
