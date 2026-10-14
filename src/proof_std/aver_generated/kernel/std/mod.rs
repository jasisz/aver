#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Tactic {
    Auto,
    Cases(std::sync::Arc<Tactic>),
    Induction(
        AverStr,
        AverStr,
        AverStr,
        std::sync::Arc<Tactic>,
        std::sync::Arc<Tactic>,
    ),
    Split(
        AverStr,
        AverStr,
        AverStr,
        std::sync::Arc<Tactic>,
        std::sync::Arc<Tactic>,
    ),
    SplitBool(
        crate::proof_std::aver_generated::kernel::term::Term,
        std::sync::Arc<Tactic>,
        std::sync::Arc<Tactic>,
    ),
    First(aver_rt::AverList<Tactic>),
    Plan(AverStr),
}

impl Tactic {
    fn aver_key_rank(&self) -> usize {
        match self {
            Tactic::Auto => 0,
            Tactic::Cases(..) => 1,
            Tactic::First(..) => 2,
            Tactic::Induction(..) => 3,
            Tactic::Plan(..) => 4,
            Tactic::Split(..) => 5,
            Tactic::SplitBool(..) => 6,
        }
    }
}

impl PartialOrd for Tactic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Tactic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Tactic::Cases(a0), Tactic::Cases(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Tactic::First(a0), Tactic::First(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Tactic::Induction(a0, a1, a2, a3, a4), Tactic::Induction(b0, b1, b2, b3, b4)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
                    .then_with(|| a3.cmp(b3))
                    .then_with(|| a4.cmp(b4))
            }
            (Tactic::Plan(a0), Tactic::Plan(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Tactic::Split(a0, a1, a2, a3, a4), Tactic::Split(b0, b1, b2, b3, b4)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
                    .then_with(|| a3.cmp(b3))
                    .then_with(|| a4.cmp(b4))
            }
            (Tactic::SplitBool(a0, a1, a2), Tactic::SplitBool(b0, b1, b2)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Tactic {
    fn aver_display(&self) -> String {
        match self {
            Tactic::Auto => "Auto".to_string(),
            Tactic::Cases(f0) => format!("Cases({})", f0.aver_display_inner()),
            Tactic::Induction(f0, f1, f2, f3, f4) => format!(
                "Induction({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner(),
                    f4.aver_display_inner()
                ]
                .join(", ")
            ),
            Tactic::Split(f0, f1, f2, f3, f4) => format!(
                "Split({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner(),
                    f4.aver_display_inner()
                ]
                .join(", ")
            ),
            Tactic::SplitBool(f0, f1, f2) => format!(
                "SplitBool({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Tactic::First(f0) => format!("First({})", f0.aver_display_inner()),
            Tactic::Plan(f0) => format!("Plan({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Origin {
    pub name: AverStr,
    pub part: AverStr,
    pub list: AverStr,
}

impl PartialOrd for Origin {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Origin {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.list.cmp(&other.list))
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.part.cmp(&other.part))
    }
}

impl aver_rt::AverDisplay for Origin {
    fn aver_display(&self) -> String {
        format!(
            "Origin({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("part: {}", self.part.aver_display_inner()),
                format!("list: {}", self.list.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Node {
    pub goal: crate::proof_std::aver_generated::kernel::proof::Goal,
    pub path: AverStr,
    pub from: aver_rt::AverList<Origin>,
    pub depth: aver_rt::AverInt,
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.depth.cmp(&other.depth))
            .then_with(|| self.from.cmp(&other.from))
            .then_with(|| self.goal.cmp(&other.goal))
            .then_with(|| self.path.cmp(&other.path))
    }
}

impl aver_rt::AverDisplay for Node {
    fn aver_display(&self) -> String {
        format!(
            "Node({})",
            vec![
                format!("goal: {}", self.goal.aver_display_inner()),
                format!("path: {}", self.path.aver_display_inner()),
                format!("from: {}", self.from.aver_display_inner()),
                format!("depth: {}", self.depth.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Reply {
    Closed(aver_rt::AverInt),
    Declined(AverStr),
    Script(Tactic),
}

impl Reply {
    fn aver_key_rank(&self) -> usize {
        match self {
            Reply::Closed(..) => 0,
            Reply::Declined(..) => 1,
            Reply::Script(..) => 2,
        }
    }
}

impl PartialOrd for Reply {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Reply {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Reply::Closed(a0), Reply::Closed(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Reply::Declined(a0), Reply::Declined(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Reply::Script(a0), Reply::Script(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Reply {
    fn aver_display(&self) -> String {
        match self {
            Reply::Closed(f0) => format!("Closed({})", f0.aver_display_inner()),
            Reply::Declined(f0) => format!("Declined({})", f0.aver_display_inner()),
            Reply::Script(f0) => format!("Script({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Answer {
    pub key: AverStr,
    pub reply: Reply,
}

impl PartialOrd for Answer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Answer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.key.cmp(&other.key))
            .then_with(|| self.reply.cmp(&other.reply))
    }
}

impl aver_rt::AverDisplay for Answer {
    fn aver_display(&self) -> String {
        format!(
            "Answer({})",
            vec![
                format!("key: {}", self.key.aver_display_inner()),
                format!("reply: {}", self.reply.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stop {
    Refused(AverStr),
    Ask(AverStr),
}

impl Stop {
    fn aver_key_rank(&self) -> usize {
        match self {
            Stop::Ask(..) => 0,
            Stop::Refused(..) => 1,
        }
    }
}

impl PartialOrd for Stop {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Stop {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Stop::Ask(a0), Stop::Ask(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Stop::Refused(a0), Stop::Refused(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Stop {
    fn aver_display(&self) -> String {
        match self {
            Stop::Refused(f0) => format!("Refused({})", f0.aver_display_inner()),
            Stop::Ask(f0) => format!("Ask({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    Within(Node, Tactic),
    Asked(Node, AverStr, AverStr),
    CasesAt(Node, Tactic),
    EachGiven(
        Node,
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
        Tactic,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    loop {
        __state = match __state {
            __MutualTco1::Within(mut n @ _, mut t @ _) => {
                crate::proof_std::cancel_checkpoint();
                match t {
                    crate::proof_std::aver_generated::kernel::std::Tactic::Auto => {
                        match crate::proof_std::aver_generated::kernel::std::refutedByWhen(&n.goal)
                        {
                            Ok(p @ _) => return Ok(p),
                            Err(_) => {
                                __MutualTco1::Asked(
                                    n.clone(),
                                    AverStr::from("Auto"),
                                    aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(21))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("auto\n"));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::std::subText(&n)))));
                                        __b
                                    }),
                                )
                            }
                        }
                    }
                    crate::proof_std::aver_generated::kernel::std::Tactic::Plan(name) => {
                        __MutualTco1::Asked(
                            n.clone(),
                            aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(22))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("Plan("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(name),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(")"));
                                __b
                            }),
                            aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(38))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("plan "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(name)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from("\n"));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::std::subText(&n)),
                                )));
                                __b
                            }),
                        )
                    }
                    crate::proof_std::aver_generated::kernel::std::Tactic::Cases(next) => {
                        let next = (*next).clone();
                        __MutualTco1::CasesAt(
                            crate::proof_std::aver_generated::kernel::std::down(
                                n,
                                AverStr::from("Cases"),
                            ),
                            next,
                        )
                    }
                    crate::proof_std::aver_generated::kernel::std::Tactic::Induction(
                        v,
                        h,
                        tl,
                        nil,
                        cons,
                    ) => {
                        let nil = (*nil).clone();
                        let cons = (*cons).clone();
                        return crate::proof_std::aver_generated::kernel::std::listStep(
                            crate::proof_std::aver_generated::kernel::std::down(
                                n,
                                aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(27))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("Induction("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(v)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(")"));
                                    __b
                                }),
                            ),
                            v,
                            h,
                            tl,
                            nil,
                            cons,
                            true,
                            &*answers,
                        );
                    }
                    crate::proof_std::aver_generated::kernel::std::Tactic::Split(
                        v,
                        h,
                        tl,
                        nil,
                        cons,
                    ) => {
                        let nil = (*nil).clone();
                        let cons = (*cons).clone();
                        return crate::proof_std::aver_generated::kernel::std::listStep(
                            crate::proof_std::aver_generated::kernel::std::down(
                                n,
                                aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(23))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("Split("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(v)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(")"));
                                    __b
                                }),
                            ),
                            v,
                            h,
                            tl,
                            nil,
                            cons,
                            false,
                            &*answers,
                        );
                    }
                    crate::proof_std::aver_generated::kernel::std::Tactic::SplitBool(
                        c,
                        yes,
                        no,
                    ) => {
                        let yes = (*yes).clone();
                        let no = (*no).clone();
                        return crate::proof_std::aver_generated::kernel::std::boolStep(
                            &crate::proof_std::aver_generated::kernel::std::down(
                                n,
                                AverStr::from("SplitBool"),
                            ),
                            &c,
                            yes,
                            no,
                            &*answers,
                        );
                    }
                    crate::proof_std::aver_generated::kernel::std::Tactic::First(ts) => {
                        return crate::proof_std::aver_generated::kernel::std::first(
                            n,
                            ts,
                            aver_rt::AverInt::from_i64(1),
                            aver_rt::AverList::empty(),
                            (*answers).clone(),
                        );
                    }
                }
            }
            __MutualTco1::Asked(mut n @ _, mut label @ _, mut key @ _) => {
                crate::proof_std::cancel_checkpoint();
                let here @ _ =
                    crate::proof_std::aver_generated::kernel::std::down(n.clone(), label);
                match crate::proof_std::aver_generated::kernel::std::lookup(
                    (*answers).clone(),
                    key.clone(),
                ) {
                    None => {
                        return Err(crate::proof_std::aver_generated::kernel::std::Stop::Ask(
                            key,
                        ));
                    }
                    Some(__pat0 @ _) => match __pat0 {
                        crate::proof_std::aver_generated::kernel::std::Reply::Closed(k) => {
                            return Ok(
                                crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(
                                    aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(17))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("#"));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(k)),
                                        ));
                                        __b
                                    }),
                                ),
                            );
                        }
                        crate::proof_std::aver_generated::kernel::std::Reply::Declined(why) => {
                            return crate::proof_std::aver_generated::kernel::std::refuse(
                                here.path, why,
                            );
                        }
                        crate::proof_std::aver_generated::kernel::std::Reply::Script(next) => {
                            if (n.depth < aver_rt::AverInt::from_i64(64)) {
                                __MutualTco1::Within(
                                    crate::proof_std::aver_generated::kernel::std::Node {
                                        depth: n.depth.add(&aver_rt::AverInt::from_i64(1)),
                                        ..here
                                    },
                                    next,
                                )
                            } else {
                                return crate::proof_std::aver_generated::kernel::std::refuse(
                                    here.path,
                                    aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(55))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from("more than "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(
                                                    &(aver_rt::AverInt::from_i64(64)),
                                                ),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(
                                            " plans run inside one another",
                                        ));
                                        __b
                                    }),
                                );
                            }
                        }
                    },
                }
            }
            __MutualTco1::CasesAt(mut n @ _, mut next @ _) => {
                crate::proof_std::cancel_checkpoint();
                let count @ _ =
                    crate::proof_std::aver_generated::kernel::std::caseCount(&n.goal.finite);
                {
                    let __list_subject = n.goal.finite.clone();
                    if __list_subject.is_empty() {
                        return crate::proof_std::aver_generated::kernel::std::refuse(
                            n.path,
                            AverStr::from("no given of finite type"),
                        );
                    } else {
                        {
                            let gs = __list_subject.clone();
                            if (count > aver_rt::AverInt::from_i64(64)) {
                                return crate::proof_std::aver_generated::kernel::std::refuse(
                                    n.path,
                                    aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(53))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(count)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(
                                            " cases is more than a split may write",
                                        ));
                                        __b
                                    }),
                                );
                            } else {
                                __MutualTco1::EachGiven(n, gs, next)
                            }
                        }
                    }
                }
            }
            __MutualTco1::EachGiven(mut n @ _, mut gs @ _, mut next @ _) => {
                crate::proof_std::cancel_checkpoint();
                let ob @ _ = n.goal.obligation.clone();
                aver_list_match!(gs, [] => __MutualTco1::Within(n, next), [g, rest] => { return Ok(crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(g.name.clone(), ob.lhs, ob.rhs, crate::proof_std::aver_generated::kernel::std::eachCase(&n, g.name.clone(), &crate::proof_std::aver_generated::kernel::finite::values(&g.fin), &rest, &next, &*answers)?)) })
            }
        };
    }
}

/// Run the script t at one node of the proof.
pub fn within(
    n @ _: Node,
    t @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    __mutual_tco_trampoline_1(__MutualTco1::Within(n, t), &answers)
}

/// A node the compiler answers: its proof #N once closed, its refusal, the script a plan gave for it, or the question when it has no answer yet.
pub fn asked(
    n @ _: Node,
    label @ _: AverStr,
    key @ _: AverStr,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    __mutual_tco_trampoline_1(__MutualTco1::Asked(n, label, key), &answers)
}

/// Split every given of finite type into its values, as the standard plan cases does, and run next in each case.
pub fn casesAt(
    n @ _: Node,
    next @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    __mutual_tco_trampoline_1(__MutualTco1::CasesAt(n, next), &answers)
}

/// Split the first given, the rest inside each of its cases; with none left, run next.
pub fn eachGiven(
    n @ _: Node,
    gs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
    next @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    __mutual_tco_trampoline_1(__MutualTco1::EachGiven(n, gs, next), &answers)
}

/// Largest number of cases a split may write, over all its givens; the automatic producer's bound.
pub fn maxCases() -> aver_rt::AverInt {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverInt::from_i64(64)
}

/// Split every given of finite type (Bool, a sum whose variants carry nothing, records and tuples of those) into all its values, in the order the kernel's enum step lists them. Each case is closed by the `when` reading false there, or else by close.
pub fn cases(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let n @ _ = crate::proof_std::aver_generated::kernel::std::caseCount(&goal.finite);
    {
        let __list_subject = goal.finite.clone();
        if __list_subject.is_empty() {
            Err(AverStr::from("no given of finite type"))
        } else {
            if (n > aver_rt::AverInt::from_i64(64)) {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" cases is more than a split may write"));
                    __b
                }))
            } else {
                crate::proof_std::aver_generated::kernel::std::splitAll(goal, &goal.finite)
            }
        }
    }
}

/// How many cases splitting every given writes.
#[inline(always)]
pub fn caseCount(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> aver_rt::AverInt {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(gs.clone(), [] => aver_rt::AverInt::from_i64(1), [g, rest] => aver_rt::AverInt::from_i64(crate::proof_std::aver_generated::kernel::finite::values(&g.fin).len() as i64).mul(&crate::proof_std::aver_generated::kernel::std::caseCount(&rest)))
}

/// Split the first given into its values and the rest inside each case; with none left, close the case.
#[inline(always)]
pub fn splitAll(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(gs.clone(), [] => crate::proof_std::aver_generated::kernel::std::leaf(goal), [g, rest] => Ok(crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(g.name.clone(), goal.obligation.lhs.clone(), goal.obligation.rhs.clone(), crate::proof_std::aver_generated::kernel::std::eachValue(goal, g.name.clone(), &crate::proof_std::aver_generated::kernel::finite::values(&g.fin), &rest)?)))
}

/// One case per value, in the kernel's order: the goal with v replaced by the value.
#[inline(always)]
pub fn eachValue(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
    vals @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    rest @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(vals.clone(), [] => Ok(aver_rt::AverList::empty()), [x, xs] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::std::splitAll(&crate::proof_std::aver_generated::kernel::std::at(goal.clone(), v.clone(), &x)?, rest)?, &crate::proof_std::aver_generated::kernel::std::eachValue(goal, v, &xs, rest)?)))
}

/// One case: impossible when the `when` reads false in it, otherwise close.
#[inline(always)]
pub fn leaf(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(goal) {
        None => crate::proof_std::aver_generated::kernel::std::close(goal),
        Some(p @ _) => {
            match crate::proof_std::aver_generated::kernel::lib::whenRefutes(
                &crate::proof_std::aver_generated::kernel::lib::scope(goal),
                &p,
                &goal.obligation.lhs,
                &goal.obligation.rhs,
            ) {
                Ok(proof @ _) => Ok(proof),
                Err(_) => crate::proof_std::aver_generated::kernel::std::close(goal),
            }
        }
    }
}

/// The goal with v replaced by x in the claim, the `when` and every hypothesis, the way a split's case reads; v is no longer a given to split.
pub fn at(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let ob @ _ = goal.obligation;
    let claim @ _ = crate::proof_std::aver_generated::kernel::proof::Law {
        lhs: crate::proof_std::aver_generated::kernel::lib::substitute(&ob.lhs, v.clone(), x)?,
        rhs: crate::proof_std::aver_generated::kernel::lib::substitute(&ob.rhs, v.clone(), x)?,
        premise: crate::proof_std::aver_generated::kernel::std::substAll(
            &ob.premise,
            v.clone(),
            x,
        )?,
        ..ob
    };
    Ok(crate::proof_std::aver_generated::kernel::proof::Goal {
        obligation: claim,
        finite: crate::proof_std::aver_generated::kernel::std::withoutGiven(goal.finite, v.clone()),
        hyps: crate::proof_std::aver_generated::kernel::std::substHyps(&goal.hyps, v, x)?,
        ..goal
    })
}

/// The finite givens other than v.
#[inline(always)]
pub fn withoutGiven(
    mut gs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(gs, [] => { return aver_rt::AverList::empty(); }, [g, rest] => { if (g.name == v) { {
            let __tco0 = rest;
            gs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(g, &crate::proof_std::aver_generated::kernel::std::withoutGiven(rest, v)); } })
    }
}

/// Each term with v replaced by x.
#[inline(always)]
pub fn substAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => Ok(aver_rt::AverList::empty()), [t, rest] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::lib::substitute(&t, v.clone(), x)?, &crate::proof_std::aver_generated::kernel::std::substAll(&rest, v, x)?)))
}

/// Each hypothesis with v replaced by x.
#[inline(always)]
pub fn substHyps(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::proof::Hyp { name: h.name, eqn: crate::proof_std::aver_generated::kernel::term::Eqn { lhs: crate::proof_std::aver_generated::kernel::lib::substitute(&h.eqn.lhs, v.clone(), x)?, rhs: crate::proof_std::aver_generated::kernel::lib::substitute(&h.eqn.rhs, v.clone(), x)? } }, &crate::proof_std::aver_generated::kernel::std::substHyps(&rest, v, x)?)))
}

/// The goal with one more hypothesis, name: lhs = rhs, in front. Only a step around the proof can bring a hypothesis into scope (a Bool split, an induction); the kernel refuses a proof that cites one it does not have.
pub fn assume(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    name @ _: AverStr,
    lhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    rhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::proof::Goal {
        hyps: aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::proof::Hyp {
                name: name,
                eqn: crate::proof_std::aver_generated::kernel::term::Eqn {
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                },
            },
            &goal.hyps,
        ),
        ..goal
    }
}

/// lhs = n = rhs when both sides evaluate to the same n, rewriting by the goal's hypotheses (left side to right side) where evaluation stops.
pub fn close(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let s @ _ = crate::proof_std::aver_generated::kernel::lib::scope(goal);
    let lhs @ _ = goal.obligation.lhs.clone();
    let rhs @ _ = goal.obligation.rhs.clone();
    let l @ _ = crate::proof_std::aver_generated::kernel::std::rounds(
        &s,
        &goal.hyps,
        &lhs,
        aver_rt::AverInt::from_i64(16),
    )?;
    let r @ _ = crate::proof_std::aver_generated::kernel::std::rounds(
        &s,
        &goal.hyps,
        &rhs,
        aver_rt::AverInt::from_i64(16),
    )?;
    if (l.to == r.to) {
        crate::proof_std::aver_generated::kernel::lib::trans(
            &aver_rt::AverList::from_vec(vec![lhs, l.to.clone(), rhs]),
            &aver_rt::AverList::from_vec(vec![
                l.proof,
                crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(std::sync::Arc::new(
                    r.proof,
                )),
            ]),
        )
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("the sides stop at "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&l.to)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" and "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::termText(&r.to)),
            )));
            __b
        }))
    }
}

/// Evaluate, rewrite once by a hypothesis, and repeat while n lasts.
pub fn rounds(
    s @ _: &crate::proof_std::aver_generated::kernel::lib::Scope,
    hyps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    n @ _: aver_rt::AverInt,
) -> Result<crate::proof_std::aver_generated::kernel::lib::Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, t)?;
    if (n > aver_rt::AverInt::from_i64(0)) {
        match crate::proof_std::aver_generated::kernel::std::rewriteOnce(
            hyps.clone(),
            r.to.clone(),
        )? {
            None => Ok(r),
            Some(step @ _) => Ok(crate::proof_std::aver_generated::kernel::lib::chain(
                t,
                &r,
                &crate::proof_std::aver_generated::kernel::lib::chain(
                    &r.to,
                    &step.clone(),
                    &crate::proof_std::aver_generated::kernel::std::rounds(
                        s,
                        hyps,
                        &step.to,
                        n.sub(&aver_rt::AverInt::from_i64(1)),
                    )?,
                ),
            )),
        }
    } else {
        Ok(r)
    }
}

/// t with its first occurrence of a hypothesis's left side replaced by its right side, by congruence.
#[inline(always)]
pub fn rewriteOnce(
    mut hyps @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    t @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<Option<crate::proof_std::aver_generated::kernel::lib::Rw>, AverStr> {
    let t @ _ = std::sync::Arc::new(t);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(hyps, [] => { return Ok(None); }, [h, rest] => { if (h.eqn.lhs == h.eqn.rhs) { {
            let __tco0 = rest;
            hyps = __tco0;
            continue;
        } } else { match crate::proof_std::aver_generated::kernel::std::holeAt(&*t, &h.eqn.lhs) { None => { {
            let __tco0 = rest;
            hyps = __tco0;
            continue;
        } }, Some(c @ _) => { return Ok(Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::subst::subst(&c, &aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("□"), &h.eqn.rhs)]))?, proof: crate::proof_std::aver_generated::kernel::std::congrHyp(&c, h.name) })); } } } })
    }
}

/// The hypothesis itself at the top, or under the context c.
pub fn congrHyp(
    c @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    name @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::proof::Proof {
    crate::proof_std::cancel_checkpoint();
    match c {
        crate::proof_std::aver_generated::kernel::term::Term::THole => {
            crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(name)
        }
        _ => crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
            c.clone(),
            std::sync::Arc::new(crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(name)),
        ),
    }
}

/// t with its first occurrence of target, outside match arms, replaced by the hole.
#[inline(always)]
pub fn holeAt(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    if (t == target) {
        Some(crate::proof_std::aver_generated::kernel::term::Term::THole)
    } else {
        crate::proof_std::aver_generated::kernel::std::holeBelow(t, target)
    }
}

/// holeAt in the parts of t.
pub fn holeBelow(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TCall(f, xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TCall(
                    f,
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(f, xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TBi(
                    f,
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TCtor(
                    c,
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TList(xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TList(
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TTuple(xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TTuple(
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TOp(o, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::proof_std::aver_generated::kernel::std::opWith(
                o,
                &a,
                &b,
                &crate::proof_std::aver_generated::kernel::std::holeAt(&a, target),
                target,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TNeg(a) => {
            let a = (*a).clone();
            crate::proof_std::aver_generated::kernel::std::negWith(
                &crate::proof_std::aver_generated::kernel::std::holeAt(&a, target),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TGet(a, f) => {
            let a = (*a).clone();
            crate::proof_std::aver_generated::kernel::std::getWith(
                &crate::proof_std::aver_generated::kernel::std::holeAt(&a, target),
                f,
            )
        }
        _ => None,
    }
}

/// shape (a call, builtin, constructor, list or tuple) around parts holding the hole.
#[inline(always)]
pub fn mapSome(
    xs @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>>,
    shape @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match xs.clone() {
        None => None,
        Some(ys @ _) => match shape.clone() {
            crate::proof_std::aver_generated::kernel::term::Term::TCall(f, _) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TCall(f, ys),
            ),
            crate::proof_std::aver_generated::kernel::term::Term::TBi(f, _) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TBi(f, ys),
            ),
            crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, _) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, ys),
            ),
            crate::proof_std::aver_generated::kernel::term::Term::TTuple(_) => {
                Some(crate::proof_std::aver_generated::kernel::term::Term::TTuple(ys))
            }
            _ => Some(crate::proof_std::aver_generated::kernel::term::Term::TList(
                ys,
            )),
        },
    }
}

/// A negation around a part holding the hole.
#[inline(always)]
pub fn negWith(
    a @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match a.clone() {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TNeg(
            std::sync::Arc::new(c),
        )),
        None => None,
    }
}

/// A field read around a part holding the hole.
#[inline(always)]
pub fn getWith(
    a @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
    f @ _: AverStr,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match a.clone() {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TGet(
            std::sync::Arc::new(c),
            f,
        )),
        None => None,
    }
}

/// An operator with the hole on the left, else on the right.
#[inline(always)]
pub fn opWith(
    o @ _: AverStr,
    a @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    left @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match left.clone() {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TOp(
            o,
            std::sync::Arc::new(c),
            std::sync::Arc::new(b.clone()),
        )),
        None => match crate::proof_std::aver_generated::kernel::std::holeAt(b, target) {
            Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TOp(
                o,
                std::sync::Arc::new(a.clone()),
                std::sync::Arc::new(c),
            )),
            None => None,
        },
    }
}

/// xs with the hole in the first element that holds target.
#[inline(always)]
pub fn holeInList(
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => None, [x, rest] => match crate::proof_std::aver_generated::kernel::std::holeAt(&x, target) { Some(c @ _) => { Some(aver_rt::AverList::prepend(c, &rest)) }, None => { crate::proof_std::aver_generated::kernel::std::prependTo(&x, &crate::proof_std::aver_generated::kernel::std::holeInList(&rest, target)) } })
}

/// x in front of a list holding the hole.
#[inline(always)]
pub fn prependTo(
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    xs @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    match xs.clone() {
        Some(ys @ _) => Some(aver_rt::AverList::prepend(x.clone(), &ys)),
        None => None,
    }
}

/// Split on the Bool term on: next proves the goal where on is true and where it is false, each time with the case as a hypothesis (fresh name: on = true or on = false) that close rewrites by.
pub fn splitBool(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    on @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    next @ _: fn(
        crate::proof_std::aver_generated::kernel::proof::Goal,
    ) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let h @ _ = crate::proof_std::aver_generated::kernel::lib::fresh(
        AverStr::from("c"),
        &crate::proof_std::aver_generated::kernel::lib::taken(&goal),
    );
    let yes @ _ = next(crate::proof_std::aver_generated::kernel::std::assume(
        goal.clone(),
        h.clone(),
        on,
        &crate::proof_std::aver_generated::kernel::term::Term::TBool(true),
    ))?;
    let no @ _ = next(crate::proof_std::aver_generated::kernel::std::assume(
        goal,
        h.clone(),
        on,
        &crate::proof_std::aver_generated::kernel::term::Term::TBool(false),
    ))?;
    Ok(
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(
            on.clone(),
            h,
            std::sync::Arc::new(yes),
            std::sync::Arc::new(no),
        ),
    )
}

/// How many Plan nodes may run inside one another before a script is refused: a plan that hands its goal back to itself must not run forever.
pub fn maxPlanDepth() -> aver_rt::AverInt {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverInt::from_i64(64)
}

/// The proof the script t writes for the goal, why it stops, or the question it needs answered first. A script does not search: it splits the goal as its nodes say, and leaves every Auto and Plan node to the compiler. The compiler answers such a node by its key (the automatic steps, or the project's plan, on the goal there) and runs the script again with the answers so far. An Auto or Plan node the compiler closed is the hypothesis #N in the proof, which the compiler replaces by its own proof N before the kernel checks the whole.
#[inline(always)]
pub fn runScript(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    mut t @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::std::within(
        crate::proof_std::aver_generated::kernel::std::Node {
            goal: goal.clone(),
            path: AverStr::from(""),
            from: aver_rt::AverList::empty(),
            depth: aver_rt::AverInt::from_i64(0),
        },
        t,
        answers,
    )
}

/// The claim in a case its `when` rules out, such as an empty list under a length bound: Auto tries this itself before it asks for the automatic steps.
#[inline(always)]
pub fn refutedByWhen(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(goal) {
        None => Err(AverStr::from("no `when`")),
        Some(p @ _) => crate::proof_std::aver_generated::kernel::lib::whenRefutes(
            &crate::proof_std::aver_generated::kernel::lib::scope(goal),
            &p,
            &goal.obligation.lhs,
            &goal.obligation.rhs,
        ),
    }
}

/// The same node, one step deeper in the script's path.
pub fn down(mut n @ _: Node, label @ _: AverStr) -> Node {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::std::Node {
        path: crate::proof_std::aver_generated::kernel::std::joinPath(n.path, label),
        ..n
    }
}

/// A path in the script one node deeper.
#[inline(always)]
pub fn joinPath(path @ _: AverStr, label @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    if (&*path == "") {
        label
    } else {
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&AverStr::from("/"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(label))));
            __b
        })
    }
}

/// A refusal that says where in the script it happened.
pub fn refuse(
    path @ _: AverStr,
    why @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    Err(
        crate::proof_std::aver_generated::kernel::std::Stop::Refused(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&AverStr::from(": "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
            __b
        })),
    )
}

/// The compiler's answer to the question key, if it gave one.
#[inline(always)]
pub fn lookup(mut answers @ _: aver_rt::AverList<Answer>, mut key @ _: AverStr) -> Option<Reply> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(answers, [] => { return None; }, [a, rest] => { if (a.key == key) { return Some(a.reply); } else { {
            let __tco0 = rest;
            answers = __tco0;
            continue;
        } } })
    }
}

/// The first script of ts that closes the node; each runs from the node with its own path, and a question stops the search until it is answered.
#[inline(always)]
pub fn first(
    n @ _: Node,
    mut ts @ _: aver_rt::AverList<Tactic>,
    mut i @ _: aver_rt::AverInt,
    mut whys @ _: aver_rt::AverList<AverStr>,
    answers @ _: aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    let n @ _ = std::sync::Arc::new(n);
    let answers @ _ = std::sync::Arc::new(answers);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ts, [] => { return Err(crate::proof_std::aver_generated::kernel::std::Stop::Refused(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::std::joinPath(n.path.clone(), AverStr::from("First")))))); __b }; __b.push_str(&AverStr::from(": none closes: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&((aver_rt::string_join(&whys.reverse(), &AverStr::from(" | "))).into_aver())))); __b }))); }, [t, rest] => { match crate::proof_std::aver_generated::kernel::std::within(crate::proof_std::aver_generated::kernel::std::Node { path: AverStr::from(""), ..(*n).clone() }, t, &*answers) { Ok(p @ _) => { return Ok(p); }, Err(__pat0 @ _) => { match __pat0 {
        crate::proof_std::aver_generated::kernel::std::Stop::Ask(key) => {
            return Err(crate::proof_std::aver_generated::kernel::std::Stop::Ask(key));
        },
        crate::proof_std::aver_generated::kernel::std::Stop::Refused(why) => {
            {
            let __tco1 = rest;
            let __tco2 = i.add(&aver_rt::AverInt::from_i64(1));
            let __tco3 = aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i)))); __b }; __b.push_str(&AverStr::from(". ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why)))); __b }), &whys);
            ts = __tco1;
            i = __tco2;
            whys = __tco3;
            continue;
        }
        }
    } } } })
    }
}

/// One case per value, in the kernel's order, its path naming the value.
#[inline(always)]
pub fn eachCase(
    n @ _: &Node,
    v @ _: AverStr,
    vals @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    rest @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
    next @ _: &Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>, Stop> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(vals.clone(), [] => Ok(aver_rt::AverList::empty()), [x, xs] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::std::eachGiven(crate::proof_std::aver_generated::kernel::std::caseNode(n.clone(), v.clone(), &x)?, rest.clone(), next.clone(), answers)?, &crate::proof_std::aver_generated::kernel::std::eachCase(n, v, &xs, rest, next, answers)?)))
}

/// The node where v is the value x.
#[inline(always)]
pub fn caseNode(
    mut n @ _: Node,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<Node, Stop> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::std::at(n.goal.clone(), v.clone(), x) {
        Ok(g @ _) => Ok({
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n,
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" = "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::std::valueText(x)),
                    )));
                    __b
                }),
            );
            crate::proof_std::aver_generated::kernel::std::Node { goal: g, ..__base }
        }),
        Err(why @ _) => Err(
            crate::proof_std::aver_generated::kernel::std::Stop::Refused(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n.path))));
                        __b
                    };
                    __b.push_str(&AverStr::from(": "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
        ),
    }
}

/// A value of a finite type as Aver writes it, for a path; a record or tuple value is only described.
pub fn valueText(x @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match x.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TBool(b) => aver_rt::AverStr::from({
            let mut __b = aver_rt::Buffer::with_capacity(
                (aver_rt::AverInt::from_i64(16)).to_usize().unwrap_or(0),
            );
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(b))));
            __b
        }),
        crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, __pat0) => {
            let __list_subject = __pat0;
            if __list_subject.is_empty() {
                c
            } else {
                AverStr::from("a value")
            }
        }
        _ => AverStr::from("a value"),
    }
}

/// Split the list variable v into [] and the cell h :: tl, v replaced in the claim, the `when` and every hypothesis; nil runs in the first case and cons in the second. As an induction, the cell case also has the claim at tl as a hypothesis (named ih, or the first free ih1, ih2, …); the kernel allows that only when no hypothesis and no `when` mentions v.
#[inline(always)]
pub fn listStep(
    mut n @ _: Node,
    v @ _: AverStr,
    h @ _: AverStr,
    tl @ _: AverStr,
    mut nil @ _: Tactic,
    mut cons @ _: Tactic,
    induct @ _: bool,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    let used @ _ = crate::proof_std::aver_generated::kernel::lib::taken(&n.goal);
    match crate::proof_std::aver_generated::kernel::std::listStepRefusal(
        &n.goal,
        v.clone(),
        h.clone(),
        tl.clone(),
        induct,
        &used,
    ) {
        Some(why @ _) => crate::proof_std::aver_generated::kernel::std::refuse(n.path, why),
        None => crate::proof_std::aver_generated::kernel::std::listCases(
            n, v, h, tl, nil, cons, induct, &used, answers,
        ),
    }
}

/// Why the list split or induction cannot be written, if it cannot.
pub fn listStepRefusal(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
    h @ _: AverStr,
    tl @ _: AverStr,
    induct @ _: bool,
    used @ _: &aver_rt::AverList<AverStr>,
) -> Option<AverStr> {
    crate::proof_std::cancel_checkpoint();
    match (
        goal.lists.contains(&v),
        (h == tl),
        used.contains(&h),
        used.contains(&tl),
    ) {
        (false, _, _, _) => Some(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                __b
            };
            __b.push_str(&AverStr::from(" is not a list variable here"));
            __b
        })),
        (_, true, _, _) => Some(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(64)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("the head and the tail need two names, not "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                __b
            };
            __b.push_str(&AverStr::from(" twice"));
            __b
        })),
        (_, _, true, _) => Some(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("the name "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                __b
            };
            __b.push_str(&AverStr::from(" is already taken"));
            __b
        })),
        (_, _, _, true) => Some(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("the name "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(tl))));
                __b
            };
            __b.push_str(&AverStr::from(" is already taken"));
            __b
        })),
        _ => {
            if (induct
                && crate::proof_std::aver_generated::kernel::std::mentionedBelow(goal, v.clone()))
            {
                Some(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(127)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("a hypothesis or the `when` mentions "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&AverStr::from(", so the claim at the tail is not a hypothesis the kernel allows; use Split"));
                    __b
                }))
            } else {
                None
            }
        }
    }
}

/// Whether the `when` or a hypothesis mentions v.
#[inline(always)]
pub fn mentionedBelow(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::subst::freeVarsAll(&aver_rt::AverList::concat(
        &goal.obligation.premise.clone(),
        &crate::proof_std::aver_generated::kernel::std::hypTerms(&goal.hyps),
    ))
    .contains(&v)
}

/// Both sides of every hypothesis.
#[inline(always)]
pub fn hypTerms(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => aver_rt::AverList::empty(), [h, rest] => aver_rt::AverList::prepend(h.eqn.lhs, &aver_rt::AverList::prepend(h.eqn.rhs, &crate::proof_std::aver_generated::kernel::std::hypTerms(&rest))))
}

/// The two cases of the split, and the step that ties them.
pub fn listCases(
    mut n @ _: Node,
    v @ _: AverStr,
    h @ _: AverStr,
    tl @ _: AverStr,
    mut nil @ _: Tactic,
    mut cons @ _: Tactic,
    induct @ _: bool,
    used @ _: &aver_rt::AverList<AverStr>,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    let ob @ _ = n.goal.obligation.clone();
    let nilGoal @ _ = crate::proof_std::aver_generated::kernel::std::withoutList(
        crate::proof_std::aver_generated::kernel::std::substituted(
            &n,
            &crate::proof_std::aver_generated::kernel::std::at(
                n.goal.clone(),
                v.clone(),
                &crate::proof_std::aver_generated::kernel::term::Term::TList(
                    aver_rt::AverList::empty(),
                ),
            ),
        )?,
        v.clone(),
    );
    let cellGoal @ _ = crate::proof_std::aver_generated::kernel::std::opened(
        crate::proof_std::aver_generated::kernel::std::substituted(
            &n,
            &crate::proof_std::aver_generated::kernel::std::at(
                n.goal.clone(),
                v.clone(),
                &crate::proof_std::aver_generated::kernel::lib::cons(
                    &crate::proof_std::aver_generated::kernel::term::Term::TVar(h.clone()),
                    &crate::proof_std::aver_generated::kernel::term::Term::TVar(tl.clone()),
                ),
            ),
        )?,
        v.clone(),
        h.clone(),
        tl.clone(),
    );
    let ih @ _ = crate::proof_std::aver_generated::kernel::lib::fresh(
        AverStr::from("ih"),
        &aver_rt::AverList::concat(
            &aver_rt::AverList::from_vec(vec![h.clone(), tl.clone()]),
            &used.clone(),
        ),
    );
    let consGoal @ _ = crate::proof_std::aver_generated::kernel::std::withIh(
        cellGoal,
        induct,
        ih.clone(),
        &ob,
        v.clone(),
        &crate::proof_std::aver_generated::kernel::term::Term::TVar(tl.clone()),
    )?;
    let cells @ _ = aver_rt::AverList::prepend(
        crate::proof_std::aver_generated::kernel::std::Origin {
            name: tl.clone(),
            part: AverStr::from("tail"),
            list: v.clone(),
        },
        &aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::std::Origin {
                name: h.clone(),
                part: AverStr::from("head"),
                list: v.clone(),
            },
            &n.from.clone(),
        ),
    );
    let nilProof @ _ = crate::proof_std::aver_generated::kernel::std::within(
        {
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n.clone(),
                AverStr::from("nil"),
            );
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: nilGoal,
                ..__base
            }
        },
        nil,
        answers,
    )?;
    let consProof @ _ = crate::proof_std::aver_generated::kernel::std::within(
        {
            let __base =
                crate::proof_std::aver_generated::kernel::std::down(n, AverStr::from("cons"));
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: consGoal,
                from: cells,
                ..__base
            }
        },
        cons,
        answers,
    )?;
    if induct {
        Ok(
            crate::proof_std::aver_generated::kernel::proof::Proof::PListInduct(
                v,
                ob.lhs,
                ob.rhs,
                aver_rt::AverList::empty(),
                std::sync::Arc::new(nilProof),
                h,
                tl,
                aver_rt::AverList::empty(),
                aver_rt::AverList::from_vec(vec![
                    crate::proof_std::aver_generated::kernel::proof::IhAt {
                        name: ih,
                        at: aver_rt::AverList::empty(),
                        carry: aver_rt::AverList::empty(),
                    },
                ]),
                std::sync::Arc::new(consProof),
            ),
        )
    } else {
        Ok(
            crate::proof_std::aver_generated::kernel::proof::Proof::PListCases(
                v,
                ob.lhs,
                ob.rhs,
                std::sync::Arc::new(nilProof),
                h,
                tl,
                std::sync::Arc::new(consProof),
            ),
        )
    }
}

/// A goal with a variable replaced, or the refusal at this node.
#[inline(always)]
pub fn substituted(
    n @ _: &Node,
    r @ _: &Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, Stop> {
    crate::proof_std::cancel_checkpoint();
    match r.clone() {
        Ok(g @ _) => Ok(g),
        Err(why @ _) => Err(
            crate::proof_std::aver_generated::kernel::std::Stop::Refused(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n.path))));
                        __b
                    };
                    __b.push_str(&AverStr::from(": "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
        ),
    }
}

/// The cell case's goal, with the claim at the tail as hypothesis ih when this is an induction.
#[inline(always)]
pub fn withIh(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    induct @ _: bool,
    ih @ _: AverStr,
    ob @ _: &crate::proof_std::aver_generated::kernel::proof::Law,
    v @ _: AverStr,
    tl @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, Stop> {
    crate::proof_std::cancel_checkpoint();
    if induct {
        crate::proof_std::aver_generated::kernel::std::withIhOf(
            goal,
            ih,
            &crate::proof_std::aver_generated::kernel::lib::substitute(&ob.lhs, v.clone(), tl),
            &crate::proof_std::aver_generated::kernel::lib::substitute(&ob.rhs, v, tl),
        )
    } else {
        Ok(goal)
    }
}

/// The goal with ih: l = r in front of its hypotheses.
pub fn withIhOf(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    ih @ _: AverStr,
    l @ _: &Result<crate::proof_std::aver_generated::kernel::term::Term, AverStr>,
    r @ _: &Result<crate::proof_std::aver_generated::kernel::term::Term, AverStr>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, Stop> {
    crate::proof_std::cancel_checkpoint();
    match (l.clone(), r.clone()) {
        (Ok(a), Ok(b)) => Ok(crate::proof_std::aver_generated::kernel::std::assume(
            goal, ih, &a, &b,
        )),
        (Err(why), _) => Err(crate::proof_std::aver_generated::kernel::std::Stop::Refused(why)),
        (_, Err(why)) => Err(crate::proof_std::aver_generated::kernel::std::Stop::Refused(why)),
    }
}

/// The goal once v is gone: no longer a given or a list variable.
pub fn withoutList(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::proof::Goal {
        obligation: crate::proof_std::aver_generated::kernel::proof::Law {
            givens: crate::proof_std::aver_generated::kernel::std::withoutVar(
                goal.obligation.givens,
                v.clone(),
            ),
            ..goal.obligation
        },
        lists: crate::proof_std::aver_generated::kernel::std::without(goal.lists, v),
        ..goal
    }
}

/// The givens other than v.
#[inline(always)]
pub fn withoutVar(
    mut gs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(gs, [] => { return aver_rt::AverList::empty(); }, [g, rest] => { if (g.name == v) { {
            let __tco0 = rest;
            gs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(g, &crate::proof_std::aver_generated::kernel::std::withoutVar(rest, v)); } })
    }
}

/// The goal of the cell case: v gone, h and tl givens in its place, h of the type v's elements have and tl of v's own.
#[inline(always)]
pub fn opened(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
    h @ _: AverStr,
    tl @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    let elem @ _ = crate::proof_std::aver_generated::kernel::std::elemType(
        goal.obligation.givens.clone(),
        v.clone(),
    );
    crate::proof_std::aver_generated::kernel::wire::withGivens(
        crate::proof_std::aver_generated::kernel::std::withoutList(goal, v),
        aver_rt::AverList::from_vec(vec![
            crate::proof_std::aver_generated::kernel::term::typed(h, &elem),
            crate::proof_std::aver_generated::kernel::term::typed(
                tl,
                &crate::proof_std::aver_generated::kernel::term::Ty::TyList(std::sync::Arc::new(
                    elem,
                )),
            ),
        ]),
    )
}

/// What the list given v holds; anything when the goal does not say.
#[inline(always)]
pub fn elemType(
    mut gs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut v @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(gs, [] => { return crate::proof_std::aver_generated::kernel::term::Ty::TyAny; }, [g, rest] => { if (g.name == v) { return crate::proof_std::aver_generated::kernel::types::elemOf(&g.ty).unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny); } else { {
            let __tco0 = rest;
            gs = __tco0;
            continue;
        } } })
    }
}

/// The names other than v.
#[inline(always)]
pub fn without(
    mut ns @ _: aver_rt::AverList<AverStr>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<AverStr> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ns, [] => { return aver_rt::AverList::empty(); }, [n, rest] => { if (n == v) { {
            let __tco0 = rest;
            ns = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(n, &crate::proof_std::aver_generated::kernel::std::without(rest, v)); } })
    }
}

/// Split on the Bool term c: yes runs where it is true and no where it is false, each case with it as a hypothesis (c, or the first free c1, c2, …).
pub fn boolStep(
    n @ _: &Node,
    c @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    mut yes @ _: Tactic,
    mut no @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    let h @ _ = crate::proof_std::aver_generated::kernel::lib::fresh(
        AverStr::from("c"),
        &crate::proof_std::aver_generated::kernel::lib::taken(&n.goal),
    );
    let t @ _ = crate::proof_std::aver_generated::kernel::std::within(
        {
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n.clone(),
                AverStr::from("true"),
            );
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: crate::proof_std::aver_generated::kernel::std::assume(
                    n.goal.clone(),
                    h.clone(),
                    c,
                    &crate::proof_std::aver_generated::kernel::term::Term::TBool(true),
                ),
                ..__base
            }
        },
        yes,
        answers,
    )?;
    let f @ _ = crate::proof_std::aver_generated::kernel::std::within(
        {
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n.clone(),
                AverStr::from("false"),
            );
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: crate::proof_std::aver_generated::kernel::std::assume(
                    n.goal.clone(),
                    h.clone(),
                    c,
                    &crate::proof_std::aver_generated::kernel::term::Term::TBool(false),
                ),
                ..__base
            }
        },
        no,
        answers,
    )?;
    Ok(
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(
            c.clone(),
            h,
            std::sync::Arc::new(t),
            std::sync::Arc::new(f),
        ),
    )
}

/// The goal at a node, as the compiler reads a question about it: (sub ((NAME TYPE)…) (from (NAME head|tail LIST)…) (hyps (NAME TERM TERM)…) PREMISE TERM TERM). The givens are typed as in an obligation; from says which list each split variable came from, oldest first; the definitions, constants and laws are the goal's own and are not repeated.
#[inline(always)]
pub fn subText(n @ _: &Node) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    let g @ _ = n.goal.clone();
    let ob @ _ = g.obligation;
    let givens @ _ = crate::proof_std::aver_generated::kernel::wire::spaced(
        &crate::proof_std::aver_generated::kernel::std::givenTexts(&ob.givens),
    );
    let from @ _ = crate::proof_std::aver_generated::kernel::wire::spaced(
        &crate::proof_std::aver_generated::kernel::std::originTexts(&n.from.reverse()),
    );
    let hyps @ _ = crate::proof_std::aver_generated::kernel::wire::spaced(
        &crate::proof_std::aver_generated::kernel::std::hypTexts(&g.hyps.reverse()),
    );
    aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(109))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&AverStr::from("(sub ("));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(givens)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(") "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::std::headedText(AverStr::from("from"), from)))));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::std::headedText(AverStr::from("hyps"), hyps)))));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::std::premiseText(
                                    &ob.premise,
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&ob.lhs)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::termText(&ob.rhs)),
            )));
            __b
        };
        __b.push_str(&AverStr::from(")"));
        __b
    })
}

/// (HEAD BODY), or (HEAD) when the body is empty.
#[inline(always)]
pub fn headedText(head @ _: AverStr, body @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    if (&*body == "") {
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("("));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        })
    } else {
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("("));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(body))));
                __b
            };
            __b.push_str(&AverStr::from(")"));
            __b
        })
    }
}

/// (none), or the `when`.
#[inline(always)]
pub fn premiseText(
    p @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = p.clone();
        if __list_subject.is_empty() {
            AverStr::from("(none)")
        } else {
            if let Some((w, _)) = aver_rt::list_uncons_cloned(&__list_subject) {
                crate::proof_std::aver_generated::kernel::wire::termText(&w)
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (NAME TYPE) for each given, as an obligation writes it.
#[inline(always)]
pub fn givenTexts(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(gs.clone(), [] => aver_rt::AverList::empty(), [g, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(g.name)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::tyText(&g.ty))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::std::givenTexts(&rest)) })
}

/// (NAME head|tail LIST) for each split variable.
#[inline(always)]
pub fn originTexts(os @ _: &aver_rt::AverList<Origin>) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(os.clone(), [] => aver_rt::AverList::empty(), [o, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(52)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o.name)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o.part)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o.list)))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::std::originTexts(&rest)) })
}

/// (NAME TERM TERM) for each hypothesis.
#[inline(always)]
pub fn hypTexts(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => aver_rt::AverList::empty(), [h, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(52)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h.name)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&h.eqn.lhs))))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&h.eqn.rhs))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::std::hypTexts(&rest)) })
}

/// The goal a question is about: base, the goal the compiler posed, with the claim, the `when`, the givens and the hypotheses that subText wrote; an empty text is base itself.
#[inline(always)]
pub fn readSub(
    mut base @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    text @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if (&*text == "") {
        Ok(base)
    } else {
        crate::proof_std::aver_generated::kernel::std::subOf(
            base,
            &crate::proof_std::aver_generated::kernel::sexp::readAll(text)?,
        )
    }
}

/// The goal of a read (sub …).
pub fn subOf(
    mut base @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s.clone() {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "sub" => {
                                let __list_subject = __pat2;
                                if let Some((__pat4, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(gs) => {
            { let __list_subject = __pat5; if let Some((__pat6, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat6 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat8) => {
            { let __list_subject = __pat8; if let Some((__pat9, _)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat9 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat10) => {
            match &*__pat10 {
        "from" => {
            { let __list_subject = __pat7; if let Some((__pat11, __pat12)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat11 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat13) => {
            { let __list_subject = __pat13; if let Some((__pat14, hs)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat14 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat15) => {
            match &*__pat15 {
        "hyps" => {
            { let __list_subject = __pat12; if let Some((p, __pat16)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat16; if let Some((l, __pat17)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat17; if let Some((r, __pat18)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat18; if __list_subject.is_empty() { crate::proof_std::aver_generated::kernel::std::subGoal(crate::proof_std::aver_generated::kernel::wire::withGivens(crate::proof_std::aver_generated::kernel::proof::Goal { obligation: crate::proof_std::aver_generated::kernel::proof::Law { givens: aver_rt::AverList::empty(), ..base.obligation }, finite: aver_rt::AverList::empty(), lists: aver_rt::AverList::empty(), ints: aver_rt::AverList::empty(), ..base }, crate::proof_std::aver_generated::kernel::decode::typedVars(&gs, false)?), &crate::proof_std::aver_generated::kernel::std::readPremise(&p)?, &crate::proof_std::aver_generated::kernel::decode::term(&l)?, &crate::proof_std::aver_generated::kernel::decode::term(&r)?, &crate::proof_std::aver_generated::kernel::std::readHyps(&hs)?) } else { Err(AverStr::from("malformed question")) } } } else { Err(AverStr::from("malformed question")) } } } else { Err(AverStr::from("malformed question")) } } } else { Err(AverStr::from("malformed question")) } }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    } } else { Err(AverStr::from("malformed question")) } }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    } } else { Err(AverStr::from("malformed question")) } }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    } } else { Err(AverStr::from("malformed question")) } }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    } } else { Err(AverStr::from("malformed question")) } }
        },
        _ => {
            Err(AverStr::from("malformed question"))
        }
    }
                                } else {
                                    Err(AverStr::from("malformed question"))
                                }
                            }
                            _ => Err(AverStr::from("malformed question")),
                        }
                    }
                    _ => Err(AverStr::from("malformed question")),
                }
            } else {
                Err(AverStr::from("malformed question"))
            }
        }
        _ => Err(AverStr::from("malformed question")),
    }
}

/// (none), or the `when`.
pub fn readPremise(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s.clone() {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "none" => {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(aver_rt::AverList::empty())
                                } else {
                                    Ok(aver_rt::AverList::from_vec(vec![
                                        crate::proof_std::aver_generated::kernel::decode::term(s)?,
                                    ]))
                                }
                            }
                            _ => Ok(aver_rt::AverList::from_vec(vec![
                                crate::proof_std::aver_generated::kernel::decode::term(s)?,
                            ])),
                        }
                    }
                    _ => Ok(aver_rt::AverList::from_vec(vec![
                        crate::proof_std::aver_generated::kernel::decode::term(s)?,
                    ])),
                }
            } else {
                Ok(aver_rt::AverList::from_vec(vec![
                    crate::proof_std::aver_generated::kernel::decode::term(s)?,
                ]))
            }
        }
        _ => Ok(aver_rt::AverList::from_vec(vec![
            crate::proof_std::aver_generated::kernel::decode::term(s)?,
        ])),
    }
}

/// g, its givens read, with the read claim and hypotheses.
pub fn subGoal(
    mut g @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    premise @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    l @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    hyps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    Ok(crate::proof_std::aver_generated::kernel::proof::Goal {
        obligation: crate::proof_std::aver_generated::kernel::proof::Law {
            premise: premise.clone(),
            lhs: l.clone(),
            rhs: r.clone(),
            ..g.obligation
        },
        hyps: hyps.clone(),
        ..g
    })
}

/// (NAME TERM TERM)…, the first the newest, as a goal lists its hypotheses.
#[inline(always)]
pub fn readHyps(
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat3; if let Some((l, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if let Some((r, __pat5)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat5; if __list_subject.is_empty() { Ok(aver_rt::AverList::concat(&crate::proof_std::aver_generated::kernel::std::readHyps(&rest)?, &aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::proof::Hyp { name: n, eqn: crate::proof_std::aver_generated::kernel::term::Eqn { lhs: crate::proof_std::aver_generated::kernel::decode::term(&l)?, rhs: crate::proof_std::aver_generated::kernel::decode::term(&r)? } }]))) } else { Err(AverStr::from("malformed hypothesis")) } } } else { Err(AverStr::from("malformed hypothesis")) } } } else { Err(AverStr::from("malformed hypothesis")) } }
        },
        _ => {
            Err(AverStr::from("malformed hypothesis"))
        }
    } } else { Err(AverStr::from("malformed hypothesis")) } }
        },
        _ => {
            Err(AverStr::from("malformed hypothesis"))
        }
    } })
}

/// A script in the step grammar: (auto), (cases T), (induction LIST HEAD TAIL T T), (split LIST HEAD TAIL T T), (splitbool TERM T T), (first T…) or (plan NAME).
pub fn tacticText(t @ _: &Tactic) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::std::Tactic::Auto => AverStr::from("(auto)"),
        crate::proof_std::aver_generated::kernel::std::Tactic::Cases(next) => {
            let next = (*next).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(cases "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::std::tacticText(&next)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::std::Tactic::Induction(v, h, tl, nil, cons) => {
            let nil = (*nil).clone();
            let cons = (*cons).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(96))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&AverStr::from("(induction "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(v)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(" "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(h)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(tl),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::std::tacticText(&nil)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::std::tacticText(&cons)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::std::Tactic::Split(v, h, tl, nil, cons) => {
            let nil = (*nil).clone();
            let cons = (*cons).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(92))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&AverStr::from("(split "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(v)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(" "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(h)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(tl),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::std::tacticText(&nil)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::std::tacticText(&cons)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::std::Tactic::SplitBool(c, yes, no) => {
            let yes = (*yes).clone();
            let no = (*no).clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(62))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("(splitbool "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::termText(
                                            &c,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::std::tacticText(&yes)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::std::tacticText(&no)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::std::Tactic::First(ts) => {
            crate::proof_std::aver_generated::kernel::std::headedText(
                AverStr::from("first"),
                crate::proof_std::aver_generated::kernel::wire::spaced(
                    &crate::proof_std::aver_generated::kernel::std::tacticTexts(&ts),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::std::Tactic::Plan(name) => {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(plan "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
    }
}

/// Each script in the step grammar.
#[inline(always)]
pub fn tacticTexts(ts @ _: &aver_rt::AverList<Tactic>) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => aver_rt::AverList::empty(), [t, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::std::tacticText(&t), &crate::proof_std::aver_generated::kernel::std::tacticTexts(&rest)))
}

/// A script tacticText wrote.
pub fn readTactic(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<Tactic, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s.clone() {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, ts)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat2) => {
                        let __dispatch_subject = __pat2;
                        if &*__dispatch_subject == "auto" {
                            {
                                let __list_subject = ts;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Auto)
                                } else {
                                    Err(AverStr::from("malformed script"))
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "cases" {
                                {
                                    let __list_subject = ts;
                                    if let Some((next, __pat3)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat3;
                                            if __list_subject.is_empty() {
                                                Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Cases(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&next)?)))
                                            } else {
                                                Err(AverStr::from("malformed script"))
                                            }
                                        }
                                    } else {
                                        Err(AverStr::from("malformed script"))
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "induction" {
                                    {
                                        let __list_subject = ts;
                                        if let Some((__pat4, __pat5)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(v) => {
            { let __list_subject = __pat5; if let Some((__pat6, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat6 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            { let __list_subject = __pat7; if let Some((__pat8, __pat9)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat8 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(tl) => {
            { let __list_subject = __pat9; if let Some((nil, __pat10)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat10; if let Some((cons, __pat11)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat11; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Induction(v, h, tl, std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&nil)?), std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&cons)?))) } else { Err(AverStr::from("malformed script")) } } } else { Err(AverStr::from("malformed script")) } } } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    } } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    } } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    }
                                        } else {
                                            Err(AverStr::from("malformed script"))
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "split" {
                                        {
                                            let __list_subject = ts;
                                            if let Some((__pat12, __pat13)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                match __pat12 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(v) => {
            { let __list_subject = __pat13; if let Some((__pat14, __pat15)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat14 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            { let __list_subject = __pat15; if let Some((__pat16, __pat17)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat16 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(tl) => {
            { let __list_subject = __pat17; if let Some((nil, __pat18)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat18; if let Some((cons, __pat19)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat19; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Split(v, h, tl, std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&nil)?), std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&cons)?))) } else { Err(AverStr::from("malformed script")) } } } else { Err(AverStr::from("malformed script")) } } } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    } } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    } } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    }
                                            } else {
                                                Err(AverStr::from("malformed script"))
                                            }
                                        }
                                    } else {
                                        if &*__dispatch_subject == "splitbool" {
                                            {
                                                let __list_subject = ts;
                                                if let Some((c, __pat20)) =
                                                    aver_rt::list_uncons_cloned(&__list_subject)
                                                {
                                                    {
                                                        let __list_subject = __pat20;
                                                        if let Some((yes, __pat21)) =
                                                            aver_rt::list_uncons_cloned(
                                                                &__list_subject,
                                                            )
                                                        {
                                                            {
                                                                let __list_subject = __pat21;
                                                                if let Some((no, __pat22)) =
                                                                    aver_rt::list_uncons_cloned(
                                                                        &__list_subject,
                                                                    )
                                                                {
                                                                    {
                                                                        let __list_subject =
                                                                            __pat22;
                                                                        if __list_subject.is_empty()
                                                                        {
                                                                            Ok(crate::proof_std::aver_generated::kernel::std::Tactic::SplitBool(crate::proof_std::aver_generated::kernel::decode::term(&c)?, std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&yes)?), std::sync::Arc::new(crate::proof_std::aver_generated::kernel::std::readTactic(&no)?)))
                                                                        } else {
                                                                            Err(AverStr::from(
                                                                                "malformed script",
                                                                            ))
                                                                        }
                                                                    }
                                                                } else {
                                                                    Err(AverStr::from(
                                                                        "malformed script",
                                                                    ))
                                                                }
                                                            }
                                                        } else {
                                                            Err(AverStr::from("malformed script"))
                                                        }
                                                    }
                                                } else {
                                                    Err(AverStr::from("malformed script"))
                                                }
                                            }
                                        } else {
                                            if &*__dispatch_subject == "first" {
                                                Ok(crate::proof_std::aver_generated::kernel::std::Tactic::First(crate::proof_std::aver_generated::kernel::std::readTactics(&ts)?))
                                            } else {
                                                if &*__dispatch_subject == "plan" {
                                                    {
                                                        let __list_subject = ts;
                                                        if let Some((__pat23, __pat24)) =
                                                            aver_rt::list_uncons_cloned(
                                                                &__list_subject,
                                                            )
                                                        {
                                                            match __pat23 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(name) => {
            { let __list_subject = __pat24; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Plan(name)) } else { Err(AverStr::from("malformed script")) } }
        },
        _ => {
            Err(AverStr::from("malformed script"))
        }
    }
                                                        } else {
                                                            Err(AverStr::from("malformed script"))
                                                        }
                                                    }
                                                } else {
                                                    Err(AverStr::from("malformed script"))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => Err(AverStr::from("malformed script")),
                }
            } else {
                Err(AverStr::from("malformed script"))
            }
        }
        _ => Err(AverStr::from("malformed script")),
    }
}

/// Each script of a list.
#[inline(always)]
pub fn readTactics(
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<Tactic>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::std::readTactic(&s)?, &crate::proof_std::aver_generated::kernel::std::readTactics(&rest)?)))
}

/// The compiler's answers so far: (answers (a "KEY" REPLY)…), REPLY (closed N), (declined "WHY") or (script T).
pub fn readAnswers(text @ _: AverStr) -> Result<aver_rt::AverList<Answer>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::sexp::readAll(text)? {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, r#as)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat2) => {
                        match &*__pat2 {
                            "answers" => {
                                crate::proof_std::aver_generated::kernel::std::answersOf(&r#as)
                            }
                            _ => Err(AverStr::from("malformed answers")),
                        }
                    }
                    _ => Err(AverStr::from("malformed answers")),
                }
            } else {
                Err(AverStr::from("malformed answers"))
            }
        }
        _ => Err(AverStr::from("malformed answers")),
    }
}

/// Each read answer.
#[inline(always)]
pub fn answersOf(
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<Answer>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat4) => {
            { let __int_match_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat4)); if __int_match_subject == aver_rt::AverInt::from_i64(97) { { let __list_subject = __pat3; if let Some((__pat5, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat5 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(k) => {
            { let __list_subject = __pat6; if let Some((r, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::std::Answer { key: k, reply: crate::proof_std::aver_generated::kernel::std::replyOf(&r)? }, &crate::proof_std::aver_generated::kernel::std::answersOf(&rest)?)) } else { Err(AverStr::from("malformed answer")) } } } else { Err(AverStr::from("malformed answer")) } }
        },
        _ => {
            Err(AverStr::from("malformed answer"))
        }
    } } else { Err(AverStr::from("malformed answer")) } } } else { Err(AverStr::from("malformed answer")) } }
        },
        _ => {
            Err(AverStr::from("malformed answer"))
        }
    } } else { Err(AverStr::from("malformed answer")) } }
        },
        _ => {
            Err(AverStr::from("malformed answer"))
        }
    } })
}

/// One reply: (closed N), (declined "WHY") or (script T).
pub fn replyOf(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<Reply, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s.clone() {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __dispatch_subject = __pat3;
                        if &*__dispatch_subject == "closed" {
                            {
                                let __list_subject = __pat2;
                                if let Some((__pat4, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat5; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Reply::Closed(({ let __s = &(n); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?)) } else { Err(AverStr::from("malformed reply")) } }
        },
        _ => {
            Err(AverStr::from("malformed reply"))
        }
    }
                                } else {
                                    Err(AverStr::from("malformed reply"))
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "declined" {
                                {
                                    let __list_subject = __pat2;
                                    if let Some((__pat6, __pat7)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        match __pat6 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(why) => {
            { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Reply::Declined(why)) } else { Err(AverStr::from("malformed reply")) } }
        },
        _ => {
            Err(AverStr::from("malformed reply"))
        }
    }
                                    } else {
                                        Err(AverStr::from("malformed reply"))
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "script" {
                                    {
                                        let __list_subject = __pat2;
                                        if let Some((t, __pat8)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat8;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_std::aver_generated::kernel::std::Reply::Script(crate::proof_std::aver_generated::kernel::std::readTactic(&t)?))
                                                } else {
                                                    Err(AverStr::from("malformed reply"))
                                                }
                                            }
                                        } else {
                                            Err(AverStr::from("malformed reply"))
                                        }
                                    }
                                } else {
                                    Err(AverStr::from("malformed reply"))
                                }
                            }
                        }
                    }
                    _ => Err(AverStr::from("malformed reply")),
                }
            } else {
                Err(AverStr::from("malformed reply"))
            }
        }
        _ => Err(AverStr::from("malformed reply")),
    }
}

/// One run of a script on the goal the compiler posed, with its answers so far: `proof` and the proof, `refused` and why, `ask` and the question to answer next, or `goal` and why the input could not be read.
#[inline(always)]
pub fn script(goalText @ _: AverStr, scriptText @ _: AverStr, answersText @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::std::readScriptInput(
        goalText,
        scriptText,
        answersText,
    ) {
        Err(why @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("goal\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
            __b
        }),
        Ok(__pat0 @ _) => {
            let (g, t, r#as) = __pat0;
            crate::proof_std::aver_generated::kernel::std::outcome(
                &crate::proof_std::aver_generated::kernel::std::runScript(&g, t, &r#as),
            )
        }
    }
}

/// The goal, the script and the answers, read.
pub fn readScriptInput(
    goalText @ _: AverStr,
    scriptText @ _: AverStr,
    answersText @ _: AverStr,
) -> Result<
    (
        crate::proof_std::aver_generated::kernel::proof::Goal,
        Tactic,
        aver_rt::AverList<Answer>,
    ),
    AverStr,
> {
    crate::proof_std::cancel_checkpoint();
    Ok((
        crate::proof_std::aver_generated::kernel::wire::goal(goalText)?,
        crate::proof_std::aver_generated::kernel::std::readTactic(
            &crate::proof_std::aver_generated::kernel::sexp::readAll(scriptText)?,
        )?,
        crate::proof_std::aver_generated::kernel::std::readAnswers(answersText)?,
    ))
}

/// What the compiler reads back from one run of a script.
#[inline(always)]
pub fn outcome(
    r @ _: &Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match r.clone() {
        Ok(p @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("proof\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::proofText(&p)),
            )));
            __b
        }),
        Err(__pat0 @ _) => match __pat0 {
            crate::proof_std::aver_generated::kernel::std::Stop::Refused(why) => {
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("refused\n"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::std::Stop::Ask(key) => {
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("ask\n"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(key))));
                    __b
                })
            }
        },
    }
}

/// What the compiler reads back from a plan that returns a script: `script` and the script, or `refused` and the plan's reason.
#[inline(always)]
pub fn scriptAnswer(r @ _: &Result<Tactic, AverStr>) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match r.clone() {
        Ok(t @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("script\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::std::tacticText(&t)),
            )));
            __b
        }),
        Err(why @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("refused\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
            __b
        }),
    }
}

/// The goal a plan runs on: the one the compiler posed, or, for a Plan node of a script, that goal as the node's question (sub) states it.
#[inline(always)]
pub fn goalAt(
    text @ _: AverStr,
    sub @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::std::readSub(
        crate::proof_std::aver_generated::kernel::wire::goal(text)?,
        sub,
    )
}
