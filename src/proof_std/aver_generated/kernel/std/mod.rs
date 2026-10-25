#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

::aver_rt::thin_sum! {
    #[derive(Debug)]
    pub Tactic / Tactic__view / Tactic__pair {
        0 Auto,
        1 Cases(Tactic__Cases),
        2 Induction(Tactic__Induction),
        3 Split(Tactic__Split),
        4 SplitBool(Tactic__SplitBool),
        5 First(Tactic__First),
        6 Plan(Tactic__Plan),
    }
}

impl std::fmt::Debug for Tactic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.view(), f)
    }
}

impl std::hash::Hash for Tactic {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Tactic {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self.view() {
            Tactic__view::Auto => {
                std::hash::Hash::hash(&0u32, state);
                left
            }
            Tactic__view::Cases(f) => {
                std::hash::Hash::hash(&1u32, state);
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                left
            }
            Tactic__view::Induction(f) => {
                std::hash::Hash::hash(&2u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                let left = aver_rt::WalkHash::stream(&f.4, state, left);
                left
            }
            Tactic__view::Split(f) => {
                std::hash::Hash::hash(&3u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                std::hash::Hash::hash(&f.0, state);
                std::hash::Hash::hash(&f.1, state);
                std::hash::Hash::hash(&f.2, state);
                let left = aver_rt::WalkHash::stream(&f.3, state, left);
                let left = aver_rt::WalkHash::stream(&f.4, state, left);
                left
            }
            Tactic__view::SplitBool(f) => {
                std::hash::Hash::hash(&4u32, state);
                if left == 0 {
                    return 0;
                }
                let left = left - 1;
                let left = aver_rt::WalkHash::stream(&f.0, state, left);
                let left = aver_rt::WalkHash::stream(&f.1, state, left);
                let left = aver_rt::WalkHash::stream(&f.2, state, left);
                left
            }
            Tactic__view::First(f) => {
                std::hash::Hash::hash(&5u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
            Tactic__view::Plan(f) => {
                std::hash::Hash::hash(&6u32, state);
                std::hash::Hash::hash(&f.0, state);
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self.view() {
            Tactic__view::Auto => aver_rt::shared_hash::tag_hash(0),
            Tactic__view::Cases(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&1u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
            }),
            Tactic__view::Induction(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&2u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.4, walk));
            }),
            Tactic__view::Split(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&3u32, h);
                std::hash::Hash::hash(&f.0, h);
                std::hash::Hash::hash(&f.1, h);
                std::hash::Hash::hash(&f.2, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.3, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.4, walk));
            }),
            Tactic__view::SplitBool(f) => walk.node(self, |walk, h| {
                std::hash::Hash::hash(&4u32, h);
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.0, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.1, walk));
                std::hash::Hasher::write_u64(h, aver_rt::WalkHash::walk_hash(&f.2, walk));
            }),
            Tactic__view::First(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&5u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
            Tactic__view::Plan(f) => walk.node(self, |_, h| {
                std::hash::Hash::hash(&6u32, h);
                std::hash::Hash::hash(&f.0, h);
            }),
        }
    }
}

impl Eq for Tactic {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Tactic__Cases(pub Tactic);

impl PartialEq for Tactic__Cases {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::WalkEq for Tactic__Cases {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Tactic__Cases {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Tactic__Induction(
    pub AverStr,
    pub AverStr,
    pub AverStr,
    pub Tactic,
    pub Tactic,
);

impl PartialEq for Tactic__Induction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0
                && self.1 == other.1
                && self.2 == other.2
                && self.3 == other.3
                && self.4 == other.4
    }
}

impl aver_rt::WalkEq for Tactic__Induction {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1 == other.1
            && self.2 == other.2
            && self.3.walk_eq(&other.3, walk)
            && self.4.walk_eq(&other.4, walk)
    }
}

impl aver_rt::thin::HandOver for Tactic__Induction {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.3, pending);
        aver_rt::thin::HandOver::hand_over(self.4, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Tactic__Split(
    pub AverStr,
    pub AverStr,
    pub AverStr,
    pub Tactic,
    pub Tactic,
);

impl PartialEq for Tactic__Split {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
            || self.0 == other.0
                && self.1 == other.1
                && self.2 == other.2
                && self.3 == other.3
                && self.4 == other.4
    }
}

impl aver_rt::WalkEq for Tactic__Split {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0 == other.0
            && self.1 == other.1
            && self.2 == other.2
            && self.3.walk_eq(&other.3, walk)
            && self.4.walk_eq(&other.4, walk)
    }
}

impl aver_rt::thin::HandOver for Tactic__Split {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.3, pending);
        aver_rt::thin::HandOver::hand_over(self.4, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Tactic__SplitBool(
    pub crate::proof_std::aver_generated::kernel::term::Term,
    pub Tactic,
    pub Tactic,
);

impl PartialEq for Tactic__SplitBool {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0 && self.1 == other.1 && self.2 == other.2
    }
}

impl aver_rt::WalkEq for Tactic__SplitBool {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        self.0.walk_eq(&other.0, walk)
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
    }
}

impl aver_rt::thin::HandOver for Tactic__SplitBool {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
    }
}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Tactic__First(pub aver_rt::AverList<Tactic>);

impl PartialEq for Tactic__First {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Tactic__First {}

#[derive(Clone, Debug, Eq, Hash)]
pub struct Tactic__Plan(pub AverStr);

impl PartialEq for Tactic__Plan {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Tactic__Plan {}

impl aver_rt::WalkEq for Tactic {
    #[inline(always)]
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        Tactic::ptr_eq(self, other) || self.aver_walk_eq_nodes(other, walk)
    }
}

impl Tactic {
    #[inline(never)]
    fn aver_walk_eq_nodes<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        use ::aver_rt::WalkEq as _;
        match Tactic::view_pair(self, other) {
            Some(Tactic__pair::Auto) => true,
            Some(Tactic__pair::Cases(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Tactic__pair::Induction(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Tactic__pair::Split(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Tactic__pair::SplitBool(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Tactic__pair::First(a0, b0)) => a0 == b0,
            Some(Tactic__pair::Plan(a0, b0)) => a0 == b0,
            None => false,
        }
    }
}

impl PartialEq for Tactic {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::walk_eq(self, other)
    }
}

impl Tactic {
    fn aver_key_rank(&self) -> usize {
        match self.view() {
            Tactic__view::Auto => 0,
            Tactic__view::Cases(..) => 1,
            Tactic__view::First(..) => 2,
            Tactic__view::Induction(..) => 3,
            Tactic__view::Plan(..) => 4,
            Tactic__view::Split(..) => 5,
            Tactic__view::SplitBool(..) => 6,
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
        match Tactic::view_pair(self, other) {
            Some(Tactic__pair::Cases(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Tactic__pair::First(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Tactic__pair::Induction(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4))),
            Some(Tactic__pair::Plan(a, b)) => {
                std::cmp::Ordering::Equal.then_with(|| (&a.0).cmp((&b.0)))
            }
            Some(Tactic__pair::Split(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2)))
                .then_with(|| (&a.3).cmp((&b.3)))
                .then_with(|| (&a.4).cmp((&b.4))),
            Some(Tactic__pair::SplitBool(a, b)) => std::cmp::Ordering::Equal
                .then_with(|| (&a.0).cmp((&b.0)))
                .then_with(|| (&a.1).cmp((&b.1)))
                .then_with(|| (&a.2).cmp((&b.2))),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Tactic {
    fn aver_display(&self) -> String {
        match self.view() {
            Tactic__view::Auto => "Auto".to_string(),
            Tactic__view::Cases(f) => format!("Cases({})", (&f.0).aver_display_inner()),
            Tactic__view::Induction(f) => format!(
                "Induction({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner()
                ]
                .join(", ")
            ),
            Tactic__view::Split(f) => format!(
                "Split({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner(),
                    (&f.3).aver_display_inner(),
                    (&f.4).aver_display_inner()
                ]
                .join(", ")
            ),
            Tactic__view::SplitBool(f) => format!(
                "SplitBool({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Tactic__view::First(f) => format!("First({})", (&f.0).aver_display_inner()),
            Tactic__view::Plan(f) => format!("Plan({})", (&f.0).aver_display_inner()),
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

#[derive(Clone, Debug, Eq, Hash)]
pub struct Node {
    pub goal: crate::proof_std::aver_generated::kernel::proof::Goal,
    pub path: AverStr,
    pub from: aver_rt::AverList<Origin>,
    pub depth: aver_rt::AverInt,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
            && self.depth == other.depth
            && self.goal == other.goal
            && self.from == other.from
    }
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
                {
                    let __t = t;
                    match __t.view() {
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::Auto => {
                            match crate::proof_std::aver_generated::kernel::std::refutedByWhen(
                                n.goal.clone(),
                            ) {
                                Ok(p @ _) => return Ok(p),
                                Err(_) => {
                                    let __arg20 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(21))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&::aver_rt::str_lit!("auto\n"));
                                            __b
                                        };
                                        __b.push_display(&(crate::proof_std::aver_generated::kernel::std::subText(&n)));
                                        __b
                                    });
                                    __MutualTco1::Asked(n, ::aver_rt::str_lit!("Auto"), __arg20)
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::Plan(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            {
                                let __arg21 @ _ = aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(22))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&::aver_rt::str_lit!("Plan("));
                                            __b
                                        };
                                        __b.push_display(&(name));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(")"));
                                    __b
                                });
                                {
                                    let __arg22 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(38))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_str(&::aver_rt::str_lit!("plan "));
                                                    __b
                                                };
                                                __b.push_display(&(name));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!("\n"));
                                            __b
                                        };
                                        __b.push_display(&(crate::proof_std::aver_generated::kernel::std::subText(&n)));
                                        __b
                                    });
                                    __MutualTco1::Asked(n, __arg21, __arg22)
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::Cases(
                            __next_fields,
                        ) => {
                            let next = &__next_fields.0;
                            {
                                let __arg23 @ _ =
                                    crate::proof_std::aver_generated::kernel::std::down(
                                        n,
                                        ::aver_rt::str_lit!("Cases"),
                                    );
                                __MutualTco1::CasesAt(__arg23, next.clone())
                            }
                        }
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::Induction(
                            __v_fields,
                        ) => {
                            let v = __v_fields.0.clone();
                            let h = __v_fields.1.clone();
                            let tl = __v_fields.2.clone();
                            let nil = &__v_fields.3;
                            let cons = &__v_fields.4;
                            {
                                let __arg25 @ _ = {
                                    let __arg24 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(27))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("Induction("));
                                                __b
                                            };
                                            __b.push_display(&(v));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(")"));
                                        __b
                                    });
                                    crate::proof_std::aver_generated::kernel::std::down(n, __arg24)
                                };
                                return crate::proof_std::aver_generated::kernel::std::listStep(
                                    __arg25,
                                    v,
                                    h,
                                    tl,
                                    nil.clone(),
                                    cons.clone(),
                                    true,
                                    &*answers,
                                );
                            }
                        }
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::Split(
                            __v_fields,
                        ) => {
                            let v = __v_fields.0.clone();
                            let h = __v_fields.1.clone();
                            let tl = __v_fields.2.clone();
                            let nil = &__v_fields.3;
                            let cons = &__v_fields.4;
                            {
                                let __arg27 @ _ = {
                                    let __arg26 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(23))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("Split("));
                                                __b
                                            };
                                            __b.push_display(&(v));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(")"));
                                        __b
                                    });
                                    crate::proof_std::aver_generated::kernel::std::down(n, __arg26)
                                };
                                return crate::proof_std::aver_generated::kernel::std::listStep(
                                    __arg27,
                                    v,
                                    h,
                                    tl,
                                    nil.clone(),
                                    cons.clone(),
                                    false,
                                    &*answers,
                                );
                            }
                        }
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::SplitBool(
                            __c_fields,
                        ) => {
                            let c = &__c_fields.0;
                            let yes = &__c_fields.1;
                            let no = &__c_fields.2;
                            {
                                let __arg28 @ _ =
                                    crate::proof_std::aver_generated::kernel::std::down(
                                        n,
                                        ::aver_rt::str_lit!("SplitBool"),
                                    );
                                return crate::proof_std::aver_generated::kernel::std::boolStep(
                                    &__arg28,
                                    c.clone(),
                                    yes.clone(),
                                    no.clone(),
                                    &*answers,
                                );
                            }
                        }
                        crate::proof_std::aver_generated::kernel::std::Tactic__view::First(
                            __ts_fields,
                        ) => {
                            let ts = &__ts_fields.0;
                            {
                                let __arg29 @ _ = <aver_rt::AverList<AverStr>>::empty();
                                return crate::proof_std::aver_generated::kernel::std::first(
                                    &n,
                                    ts,
                                    aver_rt::AverInt::from_i64(1),
                                    __arg29,
                                    &*answers,
                                );
                            }
                        }
                    }
                }
            }
            __MutualTco1::Asked(mut n @ _, mut label @ _, mut key @ _) => {
                crate::proof_std::cancel_checkpoint();
                let here @ _ =
                    crate::proof_std::aver_generated::kernel::std::down(n.clone(), label);
                match crate::proof_std::aver_generated::kernel::std::lookup(&*answers, key.clone())
                {
                    None => {
                        return Err(crate::proof_std::aver_generated::kernel::std::Stop::Ask(
                            key,
                        ));
                    }
                    Some(__pat0 @ _) => match __pat0 {
                        crate::proof_std::aver_generated::kernel::std::Reply::Closed(k) => {
                            return Ok(
                                crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(
                                    crate::proof_std::aver_generated::kernel::proof::Proof__PHyp(
                                        aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(17))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("#"));
                                                __b
                                            };
                                            __b.push_display(&(k));
                                            __b
                                        }),
                                    ),
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
                                {
                                    let __arg10 @ _ =
                                        crate::proof_std::aver_generated::kernel::std::Node {
                                            depth: n.depth.add(&aver_rt::AverInt::from_i64(1)),
                                            ..here
                                        };
                                    __MutualTco1::Within(__arg10, next)
                                }
                            } else {
                                {
                                    let __arg9 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(55))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("more than "));
                                                __b
                                            };
                                            __b.push_display(&(aver_rt::AverInt::from_i64(64)));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(
                                            " plans run inside one another"
                                        ));
                                        __b
                                    });
                                    return crate::proof_std::aver_generated::kernel::std::refuse(
                                        here.path, __arg9,
                                    );
                                }
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
                            ::aver_rt::str_lit!("no given of finite type"),
                        );
                    } else {
                        {
                            let gs = __list_subject.clone();
                            if (count > aver_rt::AverInt::from_i64(64)) {
                                {
                                    let __arg5 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(53))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_display(&(count));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(
                                            " cases is more than a split may write"
                                        ));
                                        __b
                                    });
                                    return crate::proof_std::aver_generated::kernel::std::refuse(
                                        n.path, __arg5,
                                    );
                                }
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
                aver_list_match!(gs, [] => __MutualTco1::Within(n, next), [g, rest] => { return Ok(crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(crate::proof_std::aver_generated::kernel::proof::Proof__PEnum(g.name.clone(), ob.lhs, ob.rhs, { let __arg7 @ _ = crate::proof_std::aver_generated::kernel::finite::values(&g.fin); crate::proof_std::aver_generated::kernel::std::eachCase(&n, g.name, &__arg7, &rest, &next, &*answers) }?))) })
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
            Err(::aver_rt::str_lit!("no given of finite type"))
        } else {
            if (n > aver_rt::AverInt::from_i64(64)) {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                        );
                        __b.push_display(&(n));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(
                        " cases is more than a split may write"
                    ));
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
    {
        let __list_subject = gs;
        if __list_subject.is_empty() {
            aver_rt::AverInt::from_i64(1)
        } else {
            if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverInt::from_i64(
                    crate::proof_std::aver_generated::kernel::finite::values(&g.fin).len() as i64,
                )
                .mul(&crate::proof_std::aver_generated::kernel::std::caseCount(
                    &rest,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Split the first given into its values and the rest inside each case; with none left, close the case.
#[inline(always)]
pub fn splitAll(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = gs;
        if __list_subject.is_empty() {
            crate::proof_std::aver_generated::kernel::std::leaf(goal)
        } else {
            if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(
                    crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PEnum(
                            g.name.clone(),
                            goal.obligation.lhs.clone(),
                            goal.obligation.rhs.clone(),
                            {
                                let __arg4 @ _ =
                                    crate::proof_std::aver_generated::kernel::finite::values(
                                        &g.fin,
                                    );
                                crate::proof_std::aver_generated::kernel::std::eachValue(
                                    goal,
                                    g.name.clone(),
                                    &__arg4,
                                    &rest,
                                )
                            }?,
                        ),
                    ),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    {
        let __list_subject = vals;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(x), Some(xs)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    {
                        let __arg6 @ _ = crate::proof_std::aver_generated::kernel::std::at(
                            goal.clone(),
                            v.clone(),
                            x,
                        )?;
                        crate::proof_std::aver_generated::kernel::std::splitAll(&__arg6, rest)
                    }?,
                    crate::proof_std::aver_generated::kernel::std::eachValue(goal, v, &xs, rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
            let __subject = {
                let __arg3 @ _ = crate::proof_std::aver_generated::kernel::lib::scope(goal);
                crate::proof_std::aver_generated::kernel::lib::whenRefutes(
                    &__arg3,
                    p,
                    goal.obligation.lhs.clone(),
                    goal.obligation.rhs.clone(),
                )
            };
            match __subject {
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
        lhs: crate::proof_std::aver_generated::kernel::lib::substitute(
            ob.lhs,
            v.clone(),
            x.clone(),
        )?,
        rhs: crate::proof_std::aver_generated::kernel::lib::substitute(
            ob.rhs,
            v.clone(),
            x.clone(),
        )?,
        premise: crate::proof_std::aver_generated::kernel::std::substAll(
            &ob.premise,
            v.clone(),
            x,
        )?,
        ..ob
    };
    Ok({
        let __field0 = claim;
        let __field1 =
            crate::proof_std::aver_generated::kernel::std::withoutGiven(&goal.finite, v.clone());
        let __field2 = crate::proof_std::aver_generated::kernel::std::substHyps(&goal.hyps, v, x)?;
        goal.obligation = __field0;
        goal.finite = __field1;
        goal.hyps = __field2;
        goal
    })
}

/// The finite givens other than v.
#[inline(always)]
pub fn withoutGiven(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>;
    let mut gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given> = gs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = gs;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (g.name == v) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            gs = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend_owned(
                            g.clone(),
                            crate::proof_std::aver_generated::kernel::std::withoutGiven(&rest, v),
                        );
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
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
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::lib::substitute(
                        t.clone(),
                        v.clone(),
                        x.clone(),
                    )?,
                    crate::proof_std::aver_generated::kernel::std::substAll(&rest, v, x)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Each hypothesis with v replaced by x.
#[inline(always)]
pub fn substHyps(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::proof::Hyp {
                        name: h.name.clone(),
                        eqn: crate::proof_std::aver_generated::kernel::term::Eqn {
                            lhs: crate::proof_std::aver_generated::kernel::lib::substitute(
                                h.eqn.lhs.clone(),
                                v.clone(),
                                x.clone(),
                            )?,
                            rhs: crate::proof_std::aver_generated::kernel::lib::substitute(
                                h.eqn.rhs.clone(),
                                v.clone(),
                                x.clone(),
                            )?,
                        },
                    },
                    crate::proof_std::aver_generated::kernel::std::substHyps(&rest, v, x)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The goal with one more hypothesis, name: lhs = rhs, in front. Only a step around the proof can bring a hypothesis into scope (a Bool split, an induction); the kernel refuses a proof that cites one it does not have.
pub fn assume(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    name @ _: AverStr,
    mut lhs @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut rhs @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    {
        goal.hyps = aver_rt::AverList::prepend_owned(
            crate::proof_std::aver_generated::kernel::proof::Hyp {
                name: name,
                eqn: crate::proof_std::aver_generated::kernel::term::Eqn { lhs: lhs, rhs: rhs },
            },
            goal.hyps,
        );
        goal
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
        lhs.clone(),
        aver_rt::AverInt::from_i64(16),
    )?;
    let r @ _ = crate::proof_std::aver_generated::kernel::std::rounds(
        &s,
        &goal.hyps,
        rhs.clone(),
        aver_rt::AverInt::from_i64(16),
    )?;
    if (l.to == r.to) {
        {
            let __arg6 @ _ = aver_rt::AverList::from_vec(vec![lhs, l.to.clone(), rhs]);
            {
                let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
                    l.proof,
                    crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PSymm(r.proof),
                    ),
                ]);
                crate::proof_std::aver_generated::kernel::lib::trans(&__arg6, &__arg7)
            }
        }
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("the sides stop at "));
                        __b
                    };
                    __b.push_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&l.to)),
                    );
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" and "));
                __b
            };
            __b.push_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&r.to)));
            __b
        }))
    }
}

/// Evaluate, rewrite once by a hypothesis, and repeat while n lasts.
pub fn rounds(
    s @ _: &crate::proof_std::aver_generated::kernel::lib::Scope,
    hyps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    n @ _: aver_rt::AverInt,
) -> Result<crate::proof_std::aver_generated::kernel::lib::Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, t.clone())?;
    if (n > aver_rt::AverInt::from_i64(0)) {
        match crate::proof_std::aver_generated::kernel::std::rewriteOnce(hyps, &r.to)? {
            None => Ok(r),
            Some(step @ _) => Ok({
                let __arg7 @ _ = {
                    let __arg6 @ _ = crate::proof_std::aver_generated::kernel::std::rounds(
                        s,
                        hyps,
                        step.to.clone(),
                        n.sub(&aver_rt::AverInt::from_i64(1)),
                    )?;
                    crate::proof_std::aver_generated::kernel::lib::chain(r.to.clone(), step, __arg6)
                };
                crate::proof_std::aver_generated::kernel::lib::chain(t, r, __arg7)
            }),
        }
    } else {
        Ok(r)
    }
}

/// t with its first occurrence of a hypothesis's left side replaced by its right side, by congruence.
#[inline(always)]
pub fn rewriteOnce(
    hyps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<Option<crate::proof_std::aver_generated::kernel::lib::Rw>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>;
    let mut hyps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp> =
        hyps;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = hyps;
            if __list_subject.is_empty() {
                return Ok(None);
            } else {
                if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (h.eqn.lhs == h.eqn.rhs) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            hyps = &__hold0;
                            continue;
                        }
                    } else {
                        match crate::proof_std::aver_generated::kernel::std::holeAt(t, &h.eqn.lhs) {
                            None => {
                                let __tco0 = rest;
                                __hold0 = __tco0;
                                hyps = &__hold0;
                                continue;
                            }
                            Some(c @ _) => {
                                return Ok(Some(
                                    crate::proof_std::aver_generated::kernel::lib::Rw {
                                        to: {
                                            let __arg5 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("□"), h.eqn.rhs.clone())]);
                                            crate::proof_std::aver_generated::kernel::subst::subst(
                                                c.clone(),
                                                &__arg5,
                                            )
                                        }?,
                                        proof:
                                            crate::proof_std::aver_generated::kernel::std::congrHyp(
                                                c,
                                                h.name.clone(),
                                            ),
                                    },
                                ));
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The hypothesis itself at the top, or under the context c.
pub fn congrHyp(
    mut c @ _: crate::proof_std::aver_generated::kernel::term::Term,
    name @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::proof::Proof {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = &c;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::THole => {
                crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(
                    crate::proof_std::aver_generated::kernel::proof::Proof__PHyp(name),
                )
            }
            _ => crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::proof::Proof__PCongr(
                    c,
                    crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PHyp(name),
                    ),
                ),
            ),
        }
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
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                {
                    let __arg16 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeInList(xs, target);
                    {
                        let __arg17 @ _ =
                            crate::proof_std::aver_generated::kernel::term::Term::TCall(
                                crate::proof_std::aver_generated::kernel::term::Term__TCall(
                                    f,
                                    aver_rt::AverList::empty(),
                                ),
                            );
                        crate::proof_std::aver_generated::kernel::std::mapSome(&__arg16, &__arg17)
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                {
                    let __arg18 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeInList(xs, target);
                    {
                        let __arg19 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBi(
                            crate::proof_std::aver_generated::kernel::term::Term__TBi(
                                f,
                                aver_rt::AverList::empty(),
                            ),
                        );
                        crate::proof_std::aver_generated::kernel::std::mapSome(&__arg18, &__arg19)
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                let c = __c_fields.0.clone();
                let xs = &__c_fields.1;
                {
                    let __arg20 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeInList(xs, target);
                    {
                        let __arg21 @ _ =
                            crate::proof_std::aver_generated::kernel::term::Term::TCtor(
                                crate::proof_std::aver_generated::kernel::term::Term__TCtor(
                                    c,
                                    aver_rt::AverList::empty(),
                                ),
                            );
                        crate::proof_std::aver_generated::kernel::std::mapSome(&__arg20, &__arg21)
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                let xs = &__xs_fields.0;
                {
                    let __arg22 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeInList(xs, target);
                    {
                        let __arg23 @ _ =
                            crate::proof_std::aver_generated::kernel::term::Term::TList(
                                crate::proof_std::aver_generated::kernel::term::Term__TList(
                                    aver_rt::AverList::empty(),
                                ),
                            );
                        crate::proof_std::aver_generated::kernel::std::mapSome(&__arg22, &__arg23)
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                {
                    let __arg24 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeInList(xs, target);
                    {
                        let __arg25 @ _ =
                            crate::proof_std::aver_generated::kernel::term::Term::TTuple(
                                crate::proof_std::aver_generated::kernel::term::Term__TTuple(
                                    aver_rt::AverList::empty(),
                                ),
                            );
                        crate::proof_std::aver_generated::kernel::std::mapSome(&__arg24, &__arg25)
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let a = &__o_fields.1;
                let b = &__o_fields.2;
                {
                    let __arg26 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeAt(a, target);
                    crate::proof_std::aver_generated::kernel::std::opWith(
                        o,
                        a.clone(),
                        b.clone(),
                        &__arg26,
                        target,
                    )
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                let a = &__a_fields.0;
                {
                    let __arg27 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeAt(a, target);
                    crate::proof_std::aver_generated::kernel::std::negWith(&__arg27)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TGet(__a_fields) => {
                let a = &__a_fields.0;
                let f = __a_fields.1.clone();
                {
                    let __arg28 @ _ =
                        crate::proof_std::aver_generated::kernel::std::holeAt(a, target);
                    crate::proof_std::aver_generated::kernel::std::getWith(&__arg28, f)
                }
            }
            _ => None,
        }
    }
}

/// shape (a call, builtin, constructor, list or tuple) around parts holding the hole.
#[inline(always)]
pub fn mapSome(
    xs @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>>,
    shape @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match xs {
        None => None,
        Some(ys @ _) => {
            let __t = shape;
            match __t.view() {
                crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                    let f = __f_fields.0.clone();
                    Some(crate::proof_std::aver_generated::kernel::term::Term::TCall(
                        crate::proof_std::aver_generated::kernel::term::Term__TCall(f, ys.clone()),
                    ))
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                    let f = __f_fields.0.clone();
                    Some(crate::proof_std::aver_generated::kernel::term::Term::TBi(
                        crate::proof_std::aver_generated::kernel::term::Term__TBi(f, ys.clone()),
                    ))
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                    let c = __c_fields.0.clone();
                    Some(crate::proof_std::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_std::aver_generated::kernel::term::Term__TCtor(c, ys.clone()),
                    ))
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TTuple(_) => Some(
                    crate::proof_std::aver_generated::kernel::term::Term::TTuple(
                        crate::proof_std::aver_generated::kernel::term::Term__TTuple(ys.clone()),
                    ),
                ),
                _ => Some(crate::proof_std::aver_generated::kernel::term::Term::TList(
                    crate::proof_std::aver_generated::kernel::term::Term__TList(ys.clone()),
                )),
            }
        }
    }
}

/// A negation around a part holding the hole.
#[inline(always)]
pub fn negWith(
    a @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match a {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TNeg(
            crate::proof_std::aver_generated::kernel::term::Term__TNeg(c.clone()),
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
    match a {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TGet(
            crate::proof_std::aver_generated::kernel::term::Term__TGet(c.clone(), f),
        )),
        None => None,
    }
}

/// An operator with the hole on the left, else on the right.
#[inline(always)]
pub fn opWith(
    o @ _: AverStr,
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_std::aver_generated::kernel::term::Term,
    left @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match left {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TOp(
            crate::proof_std::aver_generated::kernel::term::Term__TOp(o, c.clone(), b),
        )),
        None => match crate::proof_std::aver_generated::kernel::std::holeAt(&b, target) {
            Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TOp(
                crate::proof_std::aver_generated::kernel::term::Term__TOp(o, a, c),
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
    {
        let __list_subject = xs;
        if __list_subject.is_empty() {
            None
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match crate::proof_std::aver_generated::kernel::std::holeAt(x, target) {
                    Some(c @ _) => Some(aver_rt::AverList::prepend_owned(c, rest)),
                    None => {
                        let __arg5 @ _ = crate::proof_std::aver_generated::kernel::std::holeInList(
                            &rest, target,
                        );
                        crate::proof_std::aver_generated::kernel::std::prependTo(x, &__arg5)
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// x in front of a list holding the hole.
#[inline(always)]
pub fn prependTo(
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    xs @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    match xs {
        Some(ys @ _) => Some(aver_rt::AverList::prepend(x.clone(), ys)),
        None => None,
    }
}

/// Split on the Bool term on: next proves the goal where on is true and where it is false, each time with the case as a hypothesis (fresh name: on = true or on = false) that close rewrites by.
pub fn splitBool(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    mut on @ _: crate::proof_std::aver_generated::kernel::term::Term,
    next @ _: fn(
        crate::proof_std::aver_generated::kernel::proof::Goal,
    ) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let h @ _ = {
        let __arg6 @ _ = crate::proof_std::aver_generated::kernel::lib::taken(&goal);
        crate::proof_std::aver_generated::kernel::lib::fresh(::aver_rt::str_lit!("c"), &__arg6)
    };
    let yes @ _ = next({
        let __arg7 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBool(
            crate::proof_std::aver_generated::kernel::term::Term__TBool(true),
        );
        crate::proof_std::aver_generated::kernel::std::assume(
            goal.clone(),
            h.clone(),
            on.clone(),
            __arg7,
        )
    })?;
    let no @ _ = next({
        let __arg8 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBool(
            crate::proof_std::aver_generated::kernel::term::Term__TBool(false),
        );
        crate::proof_std::aver_generated::kernel::std::assume(goal, h.clone(), on.clone(), __arg8)
    })?;
    Ok(
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(
            crate::proof_std::aver_generated::kernel::proof::Proof__PCases(on, h, yes, no),
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
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    mut t @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    let __arg3 @ _ = crate::proof_std::aver_generated::kernel::std::Node {
        goal: goal,
        path: ::aver_rt::str_lit!(""),
        from: aver_rt::AverList::empty(),
        depth: aver_rt::AverInt::from_i64(0),
    };
    crate::proof_std::aver_generated::kernel::std::within(__arg3, t, answers)
}

/// The claim in a case its `when` rules out, such as an empty list under a length bound: Auto tries this itself before it asks for the automatic steps.
#[inline(always)]
pub fn refutedByWhen(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(&goal) {
        None => Err(::aver_rt::str_lit!("no `when`")),
        Some(p @ _) => {
            let __arg2 @ _ = crate::proof_std::aver_generated::kernel::lib::scope(&goal);
            crate::proof_std::aver_generated::kernel::lib::whenRefutes(
                &__arg2,
                p,
                goal.obligation.lhs,
                goal.obligation.rhs,
            )
        }
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
                    __b.push_display(&(path));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/"));
                __b
            };
            __b.push_display(&(label));
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
                    __b.push_display(&(path));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(": "));
                __b
            };
            __b.push_display(&(why));
            __b
        })),
    )
}

/// The compiler's answer to the question key, if it gave one.
#[inline(always)]
pub fn lookup(answers @ _: &aver_rt::AverList<Answer>, mut key @ _: AverStr) -> Option<Reply> {
    let mut __hold0: aver_rt::AverList<Answer>;
    let mut answers @ _: &aver_rt::AverList<Answer> = answers;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = answers;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (a.key == key) {
                        return Some(a.reply.clone());
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            answers = &__hold0;
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

/// The first script of ts that closes the node; each runs from the node with its own path, and a question stops the search until it is answered.
#[inline(always)]
pub fn first(
    n @ _: &Node,
    ts @ _: &aver_rt::AverList<Tactic>,
    mut i @ _: aver_rt::AverInt,
    mut whys @ _: aver_rt::AverList<AverStr>,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    let mut __hold1: aver_rt::AverList<Tactic>;
    let mut ts @ _: &aver_rt::AverList<Tactic> = ts;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ts;
            if __list_subject.is_empty() {
                return Err(
                    crate::proof_std::aver_generated::kernel::std::Stop::Refused(
                        aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_display(
                                        &(crate::proof_std::aver_generated::kernel::std::joinPath(
                                            n.path.clone(),
                                            ::aver_rt::str_lit!("First"),
                                        )),
                                    );
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(": none closes: "));
                                __b
                            };
                            __b.push_display(
                                &(aver_rt::string_join(
                                    &(whys).reverse_owned(),
                                    &::aver_rt::str_lit!(" | "),
                                )),
                            );
                            __b
                        }),
                    ),
                );
            } else {
                if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __subject = {
                            let __arg11 @ _ = crate::proof_std::aver_generated::kernel::std::Node {
                                path: ::aver_rt::str_lit!(""),
                                ..n.clone()
                            };
                            crate::proof_std::aver_generated::kernel::std::within(
                                __arg11,
                                t.clone(),
                                answers,
                            )
                        };
                        match __subject {
                            Ok(p @ _) => {
                                return Ok(p);
                            }
                            Err(__pat0 @ _) => match __pat0 {
                                crate::proof_std::aver_generated::kernel::std::Stop::Ask(key) => {
                                    return Err(
                                        crate::proof_std::aver_generated::kernel::std::Stop::Ask(
                                            key,
                                        ),
                                    );
                                }
                                crate::proof_std::aver_generated::kernel::std::Stop::Refused(
                                    why,
                                ) => {
                                    let __tco1 = rest;
                                    let __tco2 = i.add(&aver_rt::AverInt::from_i64(1));
                                    let __tco3 = aver_rt::AverList::prepend_owned(
                                        aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(34))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_display(&(i));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(". "));
                                                __b
                                            };
                                            __b.push_display(&(why));
                                            __b
                                        }),
                                        whys,
                                    );
                                    __hold1 = __tco1;
                                    ts = &__hold1;
                                    i = __tco2;
                                    whys = __tco3;
                                    continue;
                                }
                            },
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
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
    {
        let __list_subject = vals;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(x), Some(xs)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    {
                        let __arg8 @ _ = crate::proof_std::aver_generated::kernel::std::caseNode(
                            n.clone(),
                            v.clone(),
                            x,
                        )?;
                        crate::proof_std::aver_generated::kernel::std::eachGiven(
                            __arg8,
                            rest.clone(),
                            next.clone(),
                            answers,
                        )
                    }?,
                    crate::proof_std::aver_generated::kernel::std::eachCase(
                        n, v, &xs, rest, next, answers,
                    )?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
            let __base = {
                let __arg5 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0),
                            );
                            __b.push_display(&(v));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" = "));
                        __b
                    };
                    __b.push_display(
                        &(crate::proof_std::aver_generated::kernel::std::valueText(x)),
                    );
                    __b
                });
                crate::proof_std::aver_generated::kernel::std::down(n, __arg5)
            };
            crate::proof_std::aver_generated::kernel::std::Node { goal: g, ..__base }
        }),
        Err(why @ _) => Err(
            crate::proof_std::aver_generated::kernel::std::Stop::Refused(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0),
                        );
                        __b.push_display(&(n.path));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": "));
                    __b
                };
                __b.push_display(&(why));
                __b
            })),
        ),
    }
}

/// A value of a finite type as Aver writes it, for a path; a record or tuple value is only described.
pub fn valueText(x @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = x;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(16)).to_usize().unwrap_or(0),
                    );
                    __b.push_display(&(b));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                let c = __c_fields.0.clone();
                let __pat0 = &__c_fields.1;
                {
                    let __list_subject = __pat0;
                    if __list_subject.is_empty() {
                        c
                    } else {
                        ::aver_rt::str_lit!("a value")
                    }
                }
            }
            _ => {
                ::aver_rt::str_lit!("a value")
            }
        }
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
    {
        let __subject = (
            goal.lists.contains(&v),
            (h == tl),
            used.contains(&h),
            used.contains(&tl),
        );
        match __subject {
            (false, _, _, _) => Some(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                    );
                    __b.push_display(&(v));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is not a list variable here"));
                __b
            })),
            (_, true, _, _) => Some(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(64)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!(
                            "the head and the tail need two names, not "
                        ));
                        __b
                    };
                    __b.push_display(&(h));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" twice"));
                __b
            })),
            (_, _, true, _) => Some(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("the name "));
                        __b
                    };
                    __b.push_display(&(h));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is already taken"));
                __b
            })),
            (_, _, _, true) => Some(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("the name "));
                        __b
                    };
                    __b.push_display(&(tl));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is already taken"));
                __b
            })),
            _ => {
                if (induct
                    && crate::proof_std::aver_generated::kernel::std::mentionedBelow(
                        goal,
                        v.clone(),
                    ))
                {
                    Some(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(127)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!(
                                    "a hypothesis or the `when` mentions "
                                ));
                                __b
                            };
                            __b.push_display(&(v));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(", so the claim at the tail is not a hypothesis the kernel allows; use Split"));
                        __b
                    }))
                } else {
                    None
                }
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
    {
        let __arg2 @ _ = aver_rt::AverList::concat(
            &goal.obligation.premise.clone(),
            &crate::proof_std::aver_generated::kernel::std::hypTerms(&goal.hyps),
        );
        crate::proof_std::aver_generated::kernel::subst::freeVarsAll(&__arg2)
    }
    .contains(&v)
}

/// Both sides of every hypothesis.
#[inline(always)]
pub fn hypTerms(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    h.eqn.lhs.clone(),
                    aver_rt::AverList::prepend_owned(
                        h.eqn.rhs.clone(),
                        crate::proof_std::aver_generated::kernel::std::hypTerms(&rest),
                    ),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    let nilGoal @ _ = {
        let __arg19 @ _ = {
            let __arg18 @ _ = {
                let __arg17 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TList(
                    crate::proof_std::aver_generated::kernel::term::Term__TList(
                        aver_rt::AverList::empty(),
                    ),
                );
                crate::proof_std::aver_generated::kernel::std::at(
                    n.goal.clone(),
                    v.clone(),
                    &__arg17,
                )
            };
            crate::proof_std::aver_generated::kernel::std::substituted(&n, &__arg18)
        }?;
        crate::proof_std::aver_generated::kernel::std::withoutList(__arg19, v.clone())
    };
    let cellGoal @ _ = {
        let __arg24 @ _ = {
            let __arg23 @ _ = {
                let __arg22 @ _ = {
                    let __arg20 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TVar(
                        crate::proof_std::aver_generated::kernel::term::Term__TVar(h.clone()),
                    );
                    {
                        let __arg21 @ _ =
                            crate::proof_std::aver_generated::kernel::term::Term::TVar(
                                crate::proof_std::aver_generated::kernel::term::Term__TVar(
                                    tl.clone(),
                                ),
                            );
                        crate::proof_std::aver_generated::kernel::lib::cons(__arg20, __arg21)
                    }
                };
                crate::proof_std::aver_generated::kernel::std::at(
                    n.goal.clone(),
                    v.clone(),
                    &__arg22,
                )
            };
            crate::proof_std::aver_generated::kernel::std::substituted(&n, &__arg23)
        }?;
        crate::proof_std::aver_generated::kernel::std::opened(
            __arg24,
            v.clone(),
            h.clone(),
            tl.clone(),
        )
    };
    let ih @ _ = {
        let __arg25 @ _ = aver_rt::AverList::concat(
            &aver_rt::AverList::from_vec(vec![h.clone(), tl.clone()]),
            &used.clone(),
        );
        crate::proof_std::aver_generated::kernel::lib::fresh(::aver_rt::str_lit!("ih"), &__arg25)
    };
    let consGoal @ _ = {
        let __arg26 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TVar(
            crate::proof_std::aver_generated::kernel::term::Term__TVar(tl.clone()),
        );
        crate::proof_std::aver_generated::kernel::std::withIh(
            cellGoal,
            induct,
            ih.clone(),
            ob.clone(),
            v.clone(),
            __arg26,
        )
    }?;
    let cells @ _ = aver_rt::AverList::prepend_owned(
        crate::proof_std::aver_generated::kernel::std::Origin {
            name: tl.clone(),
            part: ::aver_rt::str_lit!("tail"),
            list: v.clone(),
        },
        aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::std::Origin {
                name: h.clone(),
                part: ::aver_rt::str_lit!("head"),
                list: v.clone(),
            },
            &n.from,
        ),
    );
    let nilProof @ _ = {
        let __arg27 @ _ = {
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n.clone(),
                ::aver_rt::str_lit!("nil"),
            );
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: nilGoal,
                ..__base
            }
        };
        crate::proof_std::aver_generated::kernel::std::within(__arg27, nil, answers)
    }?;
    let consProof @ _ = {
        let __arg28 @ _ = {
            let __base =
                crate::proof_std::aver_generated::kernel::std::down(n, ::aver_rt::str_lit!("cons"));
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: consGoal,
                from: cells,
                ..__base
            }
        };
        crate::proof_std::aver_generated::kernel::std::within(__arg28, cons, answers)
    }?;
    if induct {
        Ok(
            crate::proof_std::aver_generated::kernel::proof::Proof::PListInduct(
                crate::proof_std::aver_generated::kernel::proof::Proof__PListInduct(
                    v,
                    ob.lhs,
                    ob.rhs,
                    aver_rt::AverList::empty(),
                    nilProof,
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
                    consProof,
                ),
            ),
        )
    } else {
        Ok(
            crate::proof_std::aver_generated::kernel::proof::Proof::PListCases(
                crate::proof_std::aver_generated::kernel::proof::Proof__PListCases(
                    v, ob.lhs, ob.rhs, nilProof, h, tl, consProof,
                ),
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
    match r {
        Ok(g @ _) => Ok(g.clone()),
        Err(why @ _) => {
            let why = why.clone();
            Err(
                crate::proof_std::aver_generated::kernel::std::Stop::Refused(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(34)).to_usize().unwrap_or(0),
                                );
                                __b.push_display(&(n.path));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(": "));
                            __b
                        };
                        __b.push_display(&(why));
                        __b
                    }),
                ),
            )
        }
    }
}

/// The cell case's goal, with the claim at the tail as hypothesis ih when this is an induction.
#[inline(always)]
pub fn withIh(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    induct @ _: bool,
    ih @ _: AverStr,
    mut ob @ _: crate::proof_std::aver_generated::kernel::proof::Law,
    v @ _: AverStr,
    mut tl @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, Stop> {
    crate::proof_std::cancel_checkpoint();
    if induct {
        {
            let __arg6 @ _ = crate::proof_std::aver_generated::kernel::lib::substitute(
                ob.lhs,
                v.clone(),
                tl.clone(),
            );
            {
                let __arg7 @ _ =
                    crate::proof_std::aver_generated::kernel::lib::substitute(ob.rhs, v, tl);
                crate::proof_std::aver_generated::kernel::std::withIhOf(goal, ih, &__arg6, &__arg7)
            }
        }
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
            goal, ih, a, b,
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
    {
        let __field0 = crate::proof_std::aver_generated::kernel::proof::Law {
            givens: crate::proof_std::aver_generated::kernel::std::withoutVar(
                &goal.obligation.givens,
                v.clone(),
            ),
            ..goal.obligation
        };
        let __field1 = crate::proof_std::aver_generated::kernel::std::without(&goal.lists, v);
        goal.obligation = __field0;
        goal.lists = __field1;
        goal
    }
}

/// The givens other than v.
#[inline(always)]
pub fn withoutVar(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>;
    let mut gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> =
        gs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = gs;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (g.name == v) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            gs = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend_owned(
                            g.clone(),
                            crate::proof_std::aver_generated::kernel::std::withoutVar(&rest, v),
                        );
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
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
    let elem @ _ =
        crate::proof_std::aver_generated::kernel::std::elemType(&goal.obligation.givens, v.clone());
    let __arg6 @ _ = crate::proof_std::aver_generated::kernel::std::withoutList(goal, v);
    let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_std::aver_generated::kernel::term::typed(h, elem.clone()),
        {
            let __arg5 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyList(
                crate::proof_std::aver_generated::kernel::term::Ty__TyList(elem),
            );
            crate::proof_std::aver_generated::kernel::term::typed(tl, __arg5)
        },
    ]);
    crate::proof_std::aver_generated::kernel::wire::withGivens(__arg6, &__arg7)
}

/// What the list given v holds; anything when the goal does not say.
#[inline(always)]
pub fn elemType(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut v @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>;
    let mut gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> =
        gs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = gs;
            if __list_subject.is_empty() {
                return crate::proof_std::aver_generated::kernel::term::Ty::TyAny;
            } else {
                if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (g.name == v) {
                        return crate::proof_std::aver_generated::kernel::types::elemOf(&g.ty)
                            .unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny);
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            gs = &__hold0;
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

/// The names other than v.
#[inline(always)]
pub fn without(
    ns @ _: &aver_rt::AverList<AverStr>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut ns @ _: &aver_rt::AverList<AverStr> = ns;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ns;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if (n == v) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            ns = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend_owned(
                            n,
                            crate::proof_std::aver_generated::kernel::std::without(&rest, v),
                        );
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Split on the Bool term c: yes runs where it is true and no where it is false, each case with it as a hypothesis (c, or the first free c1, c2, …).
pub fn boolStep(
    n @ _: &Node,
    mut c @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut yes @ _: Tactic,
    mut no @ _: Tactic,
    answers @ _: &aver_rt::AverList<Answer>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop> {
    crate::proof_std::cancel_checkpoint();
    let h @ _ = {
        let __arg8 @ _ = crate::proof_std::aver_generated::kernel::lib::taken(&n.goal);
        crate::proof_std::aver_generated::kernel::lib::fresh(::aver_rt::str_lit!("c"), &__arg8)
    };
    let t @ _ = {
        let __arg10 @ _ = {
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n.clone(),
                ::aver_rt::str_lit!("true"),
            );
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: {
                    let __arg9 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBool(
                        crate::proof_std::aver_generated::kernel::term::Term__TBool(true),
                    );
                    crate::proof_std::aver_generated::kernel::std::assume(
                        n.goal.clone(),
                        h.clone(),
                        c.clone(),
                        __arg9,
                    )
                },
                ..__base
            }
        };
        crate::proof_std::aver_generated::kernel::std::within(__arg10, yes, answers)
    }?;
    let f @ _ = {
        let __arg12 @ _ = {
            let __base = crate::proof_std::aver_generated::kernel::std::down(
                n.clone(),
                ::aver_rt::str_lit!("false"),
            );
            crate::proof_std::aver_generated::kernel::std::Node {
                goal: {
                    let __arg11 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBool(
                        crate::proof_std::aver_generated::kernel::term::Term__TBool(false),
                    );
                    crate::proof_std::aver_generated::kernel::std::assume(
                        n.goal.clone(),
                        h.clone(),
                        c.clone(),
                        __arg11,
                    )
                },
                ..__base
            }
        };
        crate::proof_std::aver_generated::kernel::std::within(__arg12, no, answers)
    }?;
    Ok(
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(
            crate::proof_std::aver_generated::kernel::proof::Proof__PCases(c, h, t, f),
        ),
    )
}

/// The goal at a node, as the compiler reads a question about it: (sub ((NAME TYPE)…) (from (NAME head|tail LIST)…) (hyps (NAME TERM TERM)…) PREMISE TERM TERM). The givens are typed as in an obligation; from says which list each split variable came from, oldest first; the definitions, constants and laws are the goal's own and are not repeated.
#[inline(always)]
pub fn subText(n @ _: &Node) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    let g @ _ = n.goal.clone();
    let ob @ _ = g.obligation;
    let givens @ _ = {
        let __arg6 @ _ = crate::proof_std::aver_generated::kernel::std::givenTexts(&ob.givens);
        crate::proof_std::aver_generated::kernel::wire::spaced(&__arg6)
    };
    let from @ _ = {
        let __arg8 @ _ = {
            let __arg7 @ _ = n.from.reverse();
            crate::proof_std::aver_generated::kernel::std::originTexts(&__arg7)
        };
        crate::proof_std::aver_generated::kernel::wire::spaced(&__arg8)
    };
    let hyps @ _ = {
        let __arg10 @ _ = {
            let __arg9 @ _ = (g.hyps).reverse_owned();
            crate::proof_std::aver_generated::kernel::std::hypTexts(&__arg9)
        };
        crate::proof_std::aver_generated::kernel::wire::spaced(&__arg10)
    };
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
                                                        __b.push_str(&::aver_rt::str_lit!(
                                                            "(sub ("
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_display(&(givens));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(") "));
                                                __b
                                            };
                                            __b.push_display(&(crate::proof_std::aver_generated::kernel::std::headedText(::aver_rt::str_lit!("from"), from)));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" "));
                                        __b
                                    };
                                    __b.push_display(&(crate::proof_std::aver_generated::kernel::std::headedText(::aver_rt::str_lit!("hyps"), hyps)));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_display(
                                &(crate::proof_std::aver_generated::kernel::std::premiseText(
                                    &ob.premise,
                                )),
                            );
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" "));
                        __b
                    };
                    __b.push_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&ob.lhs)),
                    );
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" "));
                __b
            };
            __b.push_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&ob.rhs)));
            __b
        };
        __b.push_str(&::aver_rt::str_lit!(")"));
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
                    __b.push_str(&::aver_rt::str_lit!("("));
                    __b
                };
                __b.push_display(&(head));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(")"));
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
                            __b.push_str(&::aver_rt::str_lit!("("));
                            __b
                        };
                        __b.push_display(&(head));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" "));
                    __b
                };
                __b.push_display(&(body));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(")"));
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
        let __list_subject = p;
        if __list_subject.is_empty() {
            ::aver_rt::str_lit!("(none)")
        } else {
            if let Some(w) = __list_subject.first() {
                crate::proof_std::aver_generated::kernel::wire::termText(w)
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
    {
        let __list_subject = gs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(35))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("("));
                                        __b
                                    };
                                    __b.push_display(&(g.name));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_display(
                                &(crate::proof_std::aver_generated::kernel::wire::tyText(&g.ty)),
                            );
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    crate::proof_std::aver_generated::kernel::std::givenTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (NAME head|tail LIST) for each split variable.
#[inline(always)]
pub fn originTexts(os @ _: &aver_rt::AverList<Origin>) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = os;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(o), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(52))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("("));
                                                __b
                                            };
                                            __b.push_display(&(o.name));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" "));
                                        __b
                                    };
                                    __b.push_display(&(o.part));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_display(&(o.list));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    crate::proof_std::aver_generated::kernel::std::originTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (NAME TERM TERM) for each hypothesis.
#[inline(always)]
pub fn hypTexts(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(52))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("("));
                                                __b
                                            };
                                            __b.push_display(&(h.name));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" "));
                                        __b
                                    };
                                    __b.push_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::termText(
                                            &h.eqn.lhs,
                                        )),
                                    );
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(
                                    &h.eqn.rhs,
                                )),
                            );
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    crate::proof_std::aver_generated::kernel::std::hypTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
        {
            let __arg2 @ _ = crate::proof_std::aver_generated::kernel::sexp::readAll(text)?;
            crate::proof_std::aver_generated::kernel::std::subOf(base, &__arg2)
        }
    }
}

/// The goal of a read (sub …).
pub fn subOf(
    mut base @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "sub" => {
                                let __list_subject = __pat2;
                                if let (Some(__pat4), Some(__pat5)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(gs) => {
            { let __list_subject = __pat5; if let (Some(__pat6), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { match __pat6 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat8) => {
            { let __list_subject = __pat8; if let Some(__pat9) = __list_subject.first() { match __pat9 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat10) => {
            let __pat10 = __pat10.clone();
            match &*__pat10 {
        "from" => {
            { let __list_subject = __pat7; if let (Some(__pat11), Some(__pat12)) = (__list_subject.first(), __list_subject.tail()) { match __pat11 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat13) => {
            { let __list_subject = __pat13; if let (Some(__pat14), Some(hs)) = (__list_subject.first(), __list_subject.tail()) { match __pat14 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat15) => {
            let __pat15 = __pat15.clone();
            match &*__pat15 {
        "hyps" => {
            { let __list_subject = __pat12; if let (Some(p), Some(__pat16)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat16; if let (Some(l), Some(__pat17)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat17; if let (Some(r), Some(__pat18)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat18; if __list_subject.is_empty() { { let __arg28 @ _ = { let __arg26 @ _ = { let __field0 = crate::proof_std::aver_generated::kernel::proof::Law { givens: aver_rt::AverList::empty(), ..base.obligation }; let __field1 = aver_rt::AverList::empty(); let __field2 = aver_rt::AverList::empty(); let __field3 = aver_rt::AverList::empty(); base.obligation = __field0; base.finite = __field1; base.lists = __field2; base.ints = __field3; base }; { let __arg27 @ _ = crate::proof_std::aver_generated::kernel::decode::typedVars(gs, false)?; crate::proof_std::aver_generated::kernel::wire::withGivens(__arg26, &__arg27) } }; { let __arg29 @ _ = crate::proof_std::aver_generated::kernel::std::readPremise(p)?; { let __arg30 @ _ = crate::proof_std::aver_generated::kernel::decode::term(l)?; { let __arg31 @ _ = crate::proof_std::aver_generated::kernel::decode::term(r)?; { let __arg32 @ _ = crate::proof_std::aver_generated::kernel::std::readHyps(&hs)?; crate::proof_std::aver_generated::kernel::std::subGoal(__arg28, &__arg29, &__arg30, &__arg31, &__arg32) } } } } } } else { Err(::aver_rt::str_lit!("malformed question")) } } } else { Err(::aver_rt::str_lit!("malformed question")) } } } else { Err(::aver_rt::str_lit!("malformed question")) } } } else { Err(::aver_rt::str_lit!("malformed question")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed question")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed question")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed question")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed question")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed question"))
        }
    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed question"))
                                }
                            }
                            _ => Err(::aver_rt::str_lit!("malformed question")),
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed question")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed question"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed question")),
    }
}

/// (none), or the `when`.
pub fn readPremise(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
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
    Ok({
        let __field0 = crate::proof_std::aver_generated::kernel::proof::Law {
            premise: premise.clone(),
            lhs: l.clone(),
            rhs: r.clone(),
            ..g.obligation
        };
        let __field1 = hyps.clone();
        g.obligation = __field0;
        g.hyps = __field1;
        g
    })
}

/// (NAME TERM TERM)…, the first the newest, as a goal lists its hypotheses.
#[inline(always)]
pub fn readHyps(
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(n) => {
                                    let n = n.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(l), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if let (Some(r), Some(__pat5)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    {
                                                        let __list_subject = __pat5;
                                                        if __list_subject.is_empty() {
                                                            Ok(aver_rt::AverList::concat(&crate::proof_std::aver_generated::kernel::std::readHyps(&rest)?, &aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::proof::Hyp { name: n, eqn: crate::proof_std::aver_generated::kernel::term::Eqn { lhs: crate::proof_std::aver_generated::kernel::decode::term(l)?, rhs: crate::proof_std::aver_generated::kernel::decode::term(r)? } }])))
                                                        } else {
                                                            Err(::aver_rt::str_lit!(
                                                                "malformed hypothesis"
                                                            ))
                                                        }
                                                    }
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed hypothesis"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed hypothesis"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed hypothesis"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A script in the step grammar: (auto), (cases T), (induction LIST HEAD TAIL T T), (split LIST HEAD TAIL T T), (splitbool TERM T T), (first T…) or (plan NAME).
pub fn tacticText(t @ _: &Tactic) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::std::Tactic__view::Auto => {
                ::aver_rt::str_lit!("(auto)")
            }
            crate::proof_std::aver_generated::kernel::std::Tactic__view::Cases(__next_fields) => {
                let next = &__next_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(cases "));
                            __b
                        };
                        __b.push_display(
                            &(crate::proof_std::aver_generated::kernel::std::tacticText(next)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::std::Tactic__view::Induction(__v_fields) => {
                let v = __v_fields.0.clone();
                let h = __v_fields.1.clone();
                let tl = __v_fields.2.clone();
                let nil = &__v_fields.3;
                let cons = &__v_fields.4;
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
                                                                    (aver_rt::AverInt::from_i64(
                                                                        96,
                                                                    ))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                                );
                                                            __b.push_str(&::aver_rt::str_lit!(
                                                                "(induction "
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_display(&(v));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                                    __b
                                                };
                                                __b.push_display(&(h));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" "));
                                            __b
                                        };
                                        __b.push_display(&(tl));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_display(
                                    &(crate::proof_std::aver_generated::kernel::std::tacticText(
                                        nil,
                                    )),
                                );
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_display(
                            &(crate::proof_std::aver_generated::kernel::std::tacticText(cons)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::std::Tactic__view::Split(__v_fields) => {
                let v = __v_fields.0.clone();
                let h = __v_fields.1.clone();
                let tl = __v_fields.2.clone();
                let nil = &__v_fields.3;
                let cons = &__v_fields.4;
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
                                                                    (aver_rt::AverInt::from_i64(
                                                                        92,
                                                                    ))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                                );
                                                            __b.push_str(&::aver_rt::str_lit!(
                                                                "(split "
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_display(&(v));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                                    __b
                                                };
                                                __b.push_display(&(h));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" "));
                                            __b
                                        };
                                        __b.push_display(&(tl));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_display(
                                    &(crate::proof_std::aver_generated::kernel::std::tacticText(
                                        nil,
                                    )),
                                );
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_display(
                            &(crate::proof_std::aver_generated::kernel::std::tacticText(cons)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::std::Tactic__view::SplitBool(__c_fields) => {
                let c = &__c_fields.0;
                let yes = &__c_fields.1;
                let no = &__c_fields.2;
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
                                            __b.push_str(&::aver_rt::str_lit!("(splitbool "));
                                            __b
                                        };
                                        __b.push_display(&(crate::proof_std::aver_generated::kernel::wire::termText(c)));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_display(
                                    &(crate::proof_std::aver_generated::kernel::std::tacticText(
                                        yes,
                                    )),
                                );
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_display(
                            &(crate::proof_std::aver_generated::kernel::std::tacticText(no)),
                        );
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::std::Tactic__view::First(__ts_fields) => {
                let ts = &__ts_fields.0;
                {
                    let __arg18 @ _ = {
                        let __arg17 @ _ =
                            crate::proof_std::aver_generated::kernel::std::tacticTexts(ts);
                        crate::proof_std::aver_generated::kernel::wire::spaced(&__arg17)
                    };
                    crate::proof_std::aver_generated::kernel::std::headedText(
                        ::aver_rt::str_lit!("first"),
                        __arg18,
                    )
                }
            }
            crate::proof_std::aver_generated::kernel::std::Tactic__view::Plan(__name_fields) => {
                let name = __name_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(plan "));
                            __b
                        };
                        __b.push_display(&(name));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
        }
    }
}

/// Each script in the step grammar.
#[inline(always)]
pub fn tacticTexts(ts @ _: &aver_rt::AverList<Tactic>) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::std::tacticText(t),
                    crate::proof_std::aver_generated::kernel::std::tacticTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A script tacticText wrote.
pub fn readTactic(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<Tactic, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(ts)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat2) => {
                        let __pat2 = __pat2.clone();
                        {
                            let __dispatch_subject = __pat2;
                            if &*__dispatch_subject == "auto" {
                                {
                                    let __list_subject = ts;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Auto)
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed script"))
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "cases" {
                                    {
                                        let __list_subject = ts;
                                        if let (Some(next), Some(__pat3)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat3;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Cases(crate::proof_std::aver_generated::kernel::std::Tactic__Cases(crate::proof_std::aver_generated::kernel::std::readTactic(next)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed script"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed script"))
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "induction" {
                                        {
                                            let __list_subject = ts;
                                            if let (Some(__pat4), Some(__pat5)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(v) => {
            let v = v.clone();
            { let __list_subject = __pat5; if let (Some(__pat6), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { match __pat6 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            let h = h.clone();
            { let __list_subject = __pat7; if let (Some(__pat8), Some(__pat9)) = (__list_subject.first(), __list_subject.tail()) { match __pat8 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(tl) => {
            let tl = tl.clone();
            { let __list_subject = __pat9; if let (Some(nil), Some(__pat10)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat10; if let (Some(cons), Some(__pat11)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat11; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Induction(crate::proof_std::aver_generated::kernel::std::Tactic__Induction(v, h, tl, crate::proof_std::aver_generated::kernel::std::readTactic(nil)?, crate::proof_std::aver_generated::kernel::std::readTactic(cons)?))) } else { Err(::aver_rt::str_lit!("malformed script")) } } } else { Err(::aver_rt::str_lit!("malformed script")) } } } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed script"))
                                            }
                                        }
                                    } else {
                                        if &*__dispatch_subject == "split" {
                                            {
                                                let __list_subject = ts;
                                                if let (Some(__pat12), Some(__pat13)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    match __pat12 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(v) => {
            let v = v.clone();
            { let __list_subject = __pat13; if let (Some(__pat14), Some(__pat15)) = (__list_subject.first(), __list_subject.tail()) { match __pat14 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            let h = h.clone();
            { let __list_subject = __pat15; if let (Some(__pat16), Some(__pat17)) = (__list_subject.first(), __list_subject.tail()) { match __pat16 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(tl) => {
            let tl = tl.clone();
            { let __list_subject = __pat17; if let (Some(nil), Some(__pat18)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat18; if let (Some(cons), Some(__pat19)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat19; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Split(crate::proof_std::aver_generated::kernel::std::Tactic__Split(v, h, tl, crate::proof_std::aver_generated::kernel::std::readTactic(nil)?, crate::proof_std::aver_generated::kernel::std::readTactic(cons)?))) } else { Err(::aver_rt::str_lit!("malformed script")) } } } else { Err(::aver_rt::str_lit!("malformed script")) } } } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    }
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed script"))
                                                }
                                            }
                                        } else {
                                            if &*__dispatch_subject == "splitbool" {
                                                {
                                                    let __list_subject = ts;
                                                    if let (Some(c), Some(__pat20)) = (
                                                        __list_subject.first(),
                                                        __list_subject.tail(),
                                                    ) {
                                                        {
                                                            let __list_subject = __pat20;
                                                            if let (Some(yes), Some(__pat21)) = (
                                                                __list_subject.first(),
                                                                __list_subject.tail(),
                                                            ) {
                                                                {
                                                                    let __list_subject = __pat21;
                                                                    if let (
                                                                        Some(no),
                                                                        Some(__pat22),
                                                                    ) = (
                                                                        __list_subject.first(),
                                                                        __list_subject.tail(),
                                                                    ) {
                                                                        {
                                                                            let __list_subject =
                                                                                __pat22;
                                                                            if __list_subject
                                                                                .is_empty()
                                                                            {
                                                                                Ok(crate::proof_std::aver_generated::kernel::std::Tactic::SplitBool(crate::proof_std::aver_generated::kernel::std::Tactic__SplitBool(crate::proof_std::aver_generated::kernel::decode::term(c)?, crate::proof_std::aver_generated::kernel::std::readTactic(yes)?, crate::proof_std::aver_generated::kernel::std::readTactic(no)?)))
                                                                            } else {
                                                                                Err(
                                                                                    ::aver_rt::str_lit!(
                                                                                        "malformed script"
                                                                                    ),
                                                                                )
                                                                            }
                                                                        }
                                                                    } else {
                                                                        Err(::aver_rt::str_lit!(
                                                                            "malformed script"
                                                                        ))
                                                                    }
                                                                }
                                                            } else {
                                                                Err(::aver_rt::str_lit!(
                                                                    "malformed script"
                                                                ))
                                                            }
                                                        }
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed script"))
                                                    }
                                                }
                                            } else {
                                                if &*__dispatch_subject == "first" {
                                                    Ok(crate::proof_std::aver_generated::kernel::std::Tactic::First(crate::proof_std::aver_generated::kernel::std::Tactic__First(crate::proof_std::aver_generated::kernel::std::readTactics(&ts)?)))
                                                } else {
                                                    if &*__dispatch_subject == "plan" {
                                                        {
                                                            let __list_subject = ts;
                                                            if let (Some(__pat23), Some(__pat24)) = (
                                                                __list_subject.first(),
                                                                __list_subject.tail(),
                                                            ) {
                                                                match __pat23 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(name) => {
            let name = name.clone();
            { let __list_subject = __pat24; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Tactic::Plan(crate::proof_std::aver_generated::kernel::std::Tactic__Plan(name))) } else { Err(::aver_rt::str_lit!("malformed script")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed script"))
        }
    }
                                                            } else {
                                                                Err(::aver_rt::str_lit!(
                                                                    "malformed script"
                                                                ))
                                                            }
                                                        }
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed script"))
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed script")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed script"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed script")),
    }
}

/// Each script of a list.
#[inline(always)]
pub fn readTactics(
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<Tactic>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::std::readTactic(s)?,
                    crate::proof_std::aver_generated::kernel::std::readTactics(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The compiler's answers so far: (answers (a "KEY" REPLY)…), REPLY (closed N), (declined "WHY") or (script T).
pub fn readAnswers(text @ _: AverStr) -> Result<aver_rt::AverList<Answer>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::sexp::readAll(text)? {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(r#as)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat2) => {
                        let __pat2 = __pat2.clone();
                        match &*__pat2 {
                            "answers" => {
                                crate::proof_std::aver_generated::kernel::std::answersOf(&r#as)
                            }
                            _ => Err(::aver_rt::str_lit!("malformed answers")),
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed answers")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed answers"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed answers")),
    }
}

/// Each read answer.
#[inline(always)]
pub fn answersOf(
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<Answer>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat4,
                                ) => {
                                    let __pat4 = __pat4.clone();
                                    {
                                        let __int_match_subject =
                                            aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat4));
                                        if __int_match_subject == aver_rt::AverInt::from_i64(97) {
                                            {
                                                let __list_subject = __pat3;
                                                if let (Some(__pat5), Some(__pat6)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    match __pat5 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(k) => {
            let k = k.clone();
            { let __list_subject = __pat6; if let (Some(r), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::std::Answer { key: k, reply: crate::proof_std::aver_generated::kernel::std::replyOf(r)? }, crate::proof_std::aver_generated::kernel::std::answersOf(&rest)?)) } else { Err(::aver_rt::str_lit!("malformed answer")) } } } else { Err(::aver_rt::str_lit!("malformed answer")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed answer"))
        }
    }
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed answer"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed answer"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed answer")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed answer"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed answer")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// One reply: (closed N), (declined "WHY") or (script T).
pub fn replyOf(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<Reply, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        {
                            let __dispatch_subject = __pat3;
                            if &*__dispatch_subject == "closed" {
                                {
                                    let __list_subject = __pat2;
                                    if let (Some(__pat4), Some(__pat5)) =
                                        (__list_subject.first(), __list_subject.tail())
                                    {
                                        match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat5; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Reply::Closed(({ let __s = &(n); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?)) } else { Err(::aver_rt::str_lit!("malformed reply")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed reply"))
        }
    }
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed reply"))
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "declined" {
                                    {
                                        let __list_subject = __pat2;
                                        if let (Some(__pat6), Some(__pat7)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            match __pat6 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(why) => {
            let why = why.clone();
            { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(crate::proof_std::aver_generated::kernel::std::Reply::Declined(why)) } else { Err(::aver_rt::str_lit!("malformed reply")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed reply"))
        }
    }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed reply"))
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "script" {
                                        {
                                            let __list_subject = __pat2;
                                            if let (Some(t), Some(__pat8)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                {
                                                    let __list_subject = __pat8;
                                                    if __list_subject.is_empty() {
                                                        Ok(crate::proof_std::aver_generated::kernel::std::Reply::Script(crate::proof_std::aver_generated::kernel::std::readTactic(t)?))
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed reply"))
                                                    }
                                                }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed reply"))
                                            }
                                        }
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed reply"))
                                    }
                                }
                            }
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed reply")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed reply"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed reply")),
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
                __b.push_str(&::aver_rt::str_lit!("goal\n"));
                __b
            };
            __b.push_display(&(why));
            __b
        }),
        Ok(__pat0 @ _) => {
            let (g, t, r#as) = __pat0;
            {
                let __arg8 @ _ =
                    crate::proof_std::aver_generated::kernel::std::runScript(g, t, &r#as);
                crate::proof_std::aver_generated::kernel::std::outcome(&__arg8)
            }
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
        {
            let __arg3 @ _ = crate::proof_std::aver_generated::kernel::sexp::readAll(scriptText)?;
            crate::proof_std::aver_generated::kernel::std::readTactic(&__arg3)
        }?,
        crate::proof_std::aver_generated::kernel::std::readAnswers(answersText)?,
    ))
}

/// What the compiler reads back from one run of a script.
#[inline(always)]
pub fn outcome(
    r @ _: &Result<crate::proof_std::aver_generated::kernel::proof::Proof, Stop>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match r {
        Ok(p @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("proof\n"));
                __b
            };
            __b.push_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(p)));
            __b
        }),
        Err(__pat0 @ _) => match __pat0 {
            crate::proof_std::aver_generated::kernel::std::Stop::Refused(why) => {
                let why = why.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("refused\n"));
                        __b
                    };
                    __b.push_display(&(why));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::std::Stop::Ask(key) => {
                let key = key.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("ask\n"));
                        __b
                    };
                    __b.push_display(&(key));
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
    match r {
        Ok(t @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("script\n"));
                __b
            };
            __b.push_display(&(crate::proof_std::aver_generated::kernel::std::tacticText(t)));
            __b
        }),
        Err(why @ _) => {
            let why = why.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("refused\n"));
                    __b
                };
                __b.push_display(&(why));
                __b
            })
        }
    }
}

/// The goal a plan runs on: the one the compiler posed, or, for a Plan node of a script, that goal as the node's question (sub) states it.
#[inline(always)]
pub fn goalAt(
    text @ _: AverStr,
    sub @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let __arg2 @ _ = crate::proof_std::aver_generated::kernel::wire::goal(text)?;
    crate::proof_std::aver_generated::kernel::std::readSub(__arg2, sub)
}
