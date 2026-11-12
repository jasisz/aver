//! Octet lists: which `List<Int>` values of a program only ever hold octets
//! built in front, so that a backend may keep them as a byte buffer.
//!
//! A value flows between SLOTS: a local (parameters included) of a
//! function, a function's result, and a record field (keyed by the record's
//! name and the field's). Slots joined by a flow form one GROUP, and a group
//! is an octet group when every value that can reach it is
//!
//! - `[]`, or a literal list of `Int` literals in `0..=255`, or
//! - `List.prepend(h, x)` with `x` of the group and `h` proven in
//!   `0..=255` (see [`Ranges`]),
//!
//! and every use of it is one of
//!
//! - a flow into another slot of the group (a binding, an argument, a
//!   result, a record field),
//! - `List.len(x)`,
//! - `Bytes.fromList(List.reverse(x))`, or
//! - the tail of a `List.prepend` above.
//!
//! Any other source or use, a record holding the field being compared,
//! printed or handed to a builtin that may look at it, or a function whose
//! callers are not all known, marks the group, and the first reason is kept
//! for `AVER_EXPLAIN_OCTETS`.
//!
//! The analysis is whole-program and conservative: an expression it does
//! not model hands its values to an unknown place (they are marked) and
//! gives an unknown value (whatever it flows into is marked). Values of
//! other types flow through the same slots harmlessly; only `List<Int>`
//! slots of an octet group are ever lowered differently.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Literal, SourceLine, Spanned, Type};
use crate::ir::FnId;
use crate::ir::interval::{Bound, Interval};
use crate::ir::mir::expr::{MirCallee, MirExpr, MirPattern, MirStrPart};
use crate::ir::mir::program::LocalId;
use crate::ir::mir::{BareI64Facts, MirProgram};

/// A place a value can be held in.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Slot {
    Local(FnId, LocalId),
    Result(FnId),
    /// What a function's `Result.Ok` holds.
    ResultOk(FnId),
    Field(String, String),
}

/// The octet groups of a program: every slot of one.
#[derive(Clone, Debug, Default)]
pub struct OctetFacts {
    octet: HashSet<Slot>,
    /// Why each marked group is not one, by a slot of it (diagnostics).
    pub declined: Vec<(Slot, String)>,
}

impl OctetFacts {
    pub fn is_octet(&self, slot: &Slot) -> bool {
        self.octet.contains(slot)
    }

    pub fn local(&self, f: FnId, slot: LocalId) -> bool {
        self.octet.contains(&Slot::Local(f, slot))
    }

    pub fn result(&self, f: FnId) -> bool {
        self.octet.contains(&Slot::Result(f))
    }

    pub fn result_ok(&self, f: FnId) -> bool {
        self.octet.contains(&Slot::ResultOk(f))
    }

    pub fn field(&self, record: &str, field: &str) -> bool {
        self.octet
            .contains(&Slot::Field(record.to_string(), field.to_string()))
    }

    pub fn is_empty(&self) -> bool {
        self.octet.is_empty()
    }

    pub fn slots(&self) -> impl Iterator<Item = &Slot> {
        self.octet.iter()
    }
}

/// The types of a record's fields, by the record's name: what holding a
/// record means for the lists in it.
pub type RecordFields<'a> = &'a dyn Fn(&str) -> Option<Vec<(String, Type)>>;

/// Find the octet groups of `program`. `bare` gives the bounds of `Int`
/// values the representation analysis already proved; `fields` the field
/// types of a record. A program whose functions may have callers outside it
/// has none.
pub fn analyze(program: &MirProgram, bare: &BareI64Facts, fields: RecordFields<'_>) -> OctetFacts {
    if program.external_callers_possible {
        return OctetFacts::default();
    }
    let mut flow = Flow {
        program,
        ranges: Ranges::new(program, bare),
        nodes: HashMap::new(),
        parent: Vec::new(),
        marked: Vec::new(),
        payload: Vec::new(),
        observed: HashSet::new(),
    };
    let mut ids: Vec<FnId> = program.fns.keys().copied().collect();
    ids.sort_by_key(|id| id.0);
    for id in &ids {
        let f = &program.fns[id];
        let result = flow.node(Slot::Result(*id));
        let body = flow.eval(&f.body, *id, &mut Vec::new());
        flow.join(body, result);
    }
    // A record somebody looks at whole (compares, prints, hashes, hands to
    // a builtin that may) shows its lists as lists, and so does every
    // record inside it.
    let mut observed: Vec<String> = flow.observed.iter().cloned().collect();
    let mut seen: HashSet<String> = HashSet::new();
    while let Some(record) = observed.pop() {
        if !seen.insert(record.clone()) {
            continue;
        }
        for (field, ty) in fields(&record).unwrap_or_default() {
            let slot = flow.node(Slot::Field(record.clone(), field.clone()));
            flow.mark(slot, format!("a {record} is looked at whole"));
            named_types(&ty, &mut |name| observed.push(name.to_string()));
        }
    }
    // What a `Result` holds goes wherever the `Result` goes.
    loop {
        let mut changed = false;
        for n in 0..flow.parent.len() {
            let root = flow.find(n);
            if root != n || flow.marked[root].is_empty() {
                continue;
            }
            if let Some(inner) = flow.payload[root] {
                let inner = flow.find(inner);
                if flow.marked[inner].is_empty() {
                    let why = format!("held in a Result that {}", flow.marked[root][0]);
                    flow.mark(inner, why);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    for id in &ids {
        let result = flow.node(Slot::Result(*id));
        let root = flow.find(result);
        if flow.payload[root].is_some() {
            let ok = flow.payload_of(result);
            let slot = flow.node(Slot::ResultOk(*id));
            flow.union(ok, slot);
        }
    }
    let mut facts = OctetFacts::default();
    let mut reported: HashSet<usize> = HashSet::new();
    let entries: Vec<(Slot, usize)> = flow.nodes.iter().map(|(s, n)| (s.clone(), *n)).collect();
    for (slot, node) in entries {
        let root = flow.find(node);
        match flow.marked[root].as_slice() {
            [] => {
                facts.octet.insert(slot);
            }
            whys => {
                let _ = reported.insert(root);
                facts.declined.push((slot, whys.join("; ")));
            }
        }
    }
    if std::env::var_os("AVER_EXPLAIN_OCTETS").is_some() {
        explain(program, &facts, fields);
    }
    facts
}

fn explain(program: &MirProgram, facts: &OctetFacts, fields: RecordFields<'_>) {
    let int_list = Type::List(Box::new(Type::Int));
    let name = |slot: &Slot| match slot {
        Slot::Local(f, l) => {
            let f = program.fn_by_id(*f);
            let param = f.and_then(|f| f.params.iter().find(|p| p.local == *l));
            match (f, param) {
                (Some(f), Some(p)) => format!("{}({})", f.name, p.name),
                (Some(f), None) => format!("{} local #{}", f.name, l.0),
                _ => format!("local #{}", l.0),
            }
        }
        Slot::Result(f) => format!(
            "{} result",
            program.fn_by_id(*f).map_or("?", |f| f.name.as_str())
        ),
        Slot::ResultOk(f) => format!(
            "{} Ok",
            program.fn_by_id(*f).map_or("?", |f| f.name.as_str())
        ),
        Slot::Field(r, f) => format!("{r}.{f}"),
    };
    let listed = |s: &&Slot| match s {
        Slot::Local(f, l) => program.fn_by_id(*f).is_some_and(|f| {
            f.params
                .iter()
                .any(|p| p.local == *l && p.ty == format!("{int_list:?}"))
        }),
        Slot::Result(f) => program
            .fn_by_id(*f)
            .is_some_and(|f| f.return_type == format!("{int_list:?}")),
        Slot::ResultOk(f) => program
            .fn_by_id(*f)
            .is_some_and(|f| f.return_type.starts_with(&format!("Result({int_list:?}, "))),
        Slot::Field(r, name) => fields(r)
            .unwrap_or_default()
            .iter()
            .any(|(n, t)| n == name && *t == int_list),
    };
    let mut octet: Vec<String> = facts.octet.iter().filter(listed).map(name).collect();
    octet.sort();
    eprintln!("octets: {}", octet.join(", "));
    let mut declined: Vec<String> = facts
        .declined
        .iter()
        .filter(|(slot, _)| listed(&slot))
        .map(|(slot, why)| format!("octets declined at {}: {why}", name(slot)))
        .collect();
    declined.sort();
    for line in declined {
        eprintln!("{line}");
    }
}

/// A value an expression gives.
#[derive(Clone, Debug)]
enum Val {
    /// Held by a group.
    Node(usize),
    /// A fresh value of no group: `[]`, an octet literal, a scalar.
    Fresh,
    /// A value from a place the analysis does not follow.
    Unknown(String),
}

struct Flow<'p> {
    program: &'p MirProgram,
    ranges: Ranges<'p>,
    nodes: HashMap<Slot, usize>,
    parent: Vec<usize>,
    /// Why a group is not an octet one: every distinct reason, in order.
    marked: Vec<Vec<String>>,
    /// The group a `Result` group's `Ok` values are of, by root.
    payload: Vec<Option<usize>>,
    /// Records looked at whole.
    observed: HashSet<String>,
}

/// What an enclosing branch proved about an `Int`: the read it bounds
/// (a local and a path of fields) and the bound.
type Known = Vec<(Path, Interval)>;
type Path = (LocalId, Vec<String>);

impl<'p> Flow<'p> {
    fn node(&mut self, slot: Slot) -> usize {
        if let Some(n) = self.nodes.get(&slot) {
            return *n;
        }
        let n = self.fresh_node();
        self.nodes.insert(slot, n);
        n
    }

    fn fresh_node(&mut self) -> usize {
        let n = self.parent.len();
        self.parent.push(n);
        self.marked.push(Vec::new());
        self.payload.push(None);
        n
    }

    /// The group what `n`'s `Result.Ok` values hold is.
    fn payload_of(&mut self, n: usize) -> usize {
        let root = self.find(n);
        if let Some(inner) = self.payload[root] {
            return inner;
        }
        let inner = self.fresh_node();
        self.payload[root] = Some(inner);
        inner
    }

    fn find(&mut self, mut n: usize) -> usize {
        while self.parent[n] != n {
            self.parent[n] = self.parent[self.parent[n]];
            n = self.parent[n];
        }
        n
    }

    fn union(&mut self, a: usize, b: usize) {
        let (a, b) = (self.find(a), self.find(b));
        if a == b {
            return;
        }
        self.parent[b] = a;
        let moved = std::mem::take(&mut self.marked[b]);
        for why in moved {
            if !self.marked[a].contains(&why) {
                self.marked[a].push(why);
            }
        }
        // Two `Result` groups made one hold one group.
        match (self.payload[a], self.payload[b].take()) {
            (Some(x), Some(y)) => self.union(x, y),
            (None, inner) => self.payload[a] = inner,
            _ => {}
        }
    }

    fn mark(&mut self, n: usize, why: String) {
        let root = self.find(n);
        if !self.marked[root].contains(&why) {
            self.marked[root].push(why);
        }
    }

    /// `v` flows into group `into`.
    fn join(&mut self, v: Val, into: usize) {
        match v {
            Val::Node(n) => self.union(n, into),
            Val::Fresh => {}
            Val::Unknown(why) => self.mark(into, why),
        }
    }

    /// `v` goes where the analysis does not follow.
    fn sink(&mut self, v: Val, why: impl FnOnce() -> String) {
        if let Val::Node(n) = v {
            self.mark(n, why());
        }
    }

    fn builtin(&self, callee: &MirCallee) -> Option<&'p str> {
        match callee {
            MirCallee::Builtin(id) => Some(self.program.builtin_name(*id)),
            _ => None,
        }
    }

    fn eval(&mut self, e: &Spanned<MirExpr>, f: FnId, known: &mut Known) -> Val {
        let line = e.line;
        match &e.node {
            MirExpr::Literal(_) => Val::Fresh,
            MirExpr::Local(l) => Val::Node(self.node(Slot::Local(f, l.node.slot))),
            MirExpr::List(items) => {
                let octets = items.iter().all(|i| {
                    matches!(&i.node, MirExpr::Literal(l) if matches!(l.node, Literal::Int(k) if (0..=255).contains(&k)))
                });
                for i in items {
                    let v = self.eval(i, f, known);
                    self.sink(v, || format!("held in a list literal (line {line})"));
                }
                match octets {
                    true => Val::Fresh,
                    false => Val::Unknown(format!("a list literal of other values (line {line})")),
                }
            }
            MirExpr::Project(p) => {
                // Reading a field does not look at the record's other ones.
                let _ = self.eval(&p.node.base, f, known);
                match record_name(p.node.base.ty()) {
                    Some(record) => Val::Node(self.node(Slot::Field(record, p.node.field.clone()))),
                    None => Val::Unknown(format!("a field of an unnamed value (line {line})")),
                }
            }
            MirExpr::Let(l) => {
                let v = self.eval(&l.node.value, f, known);
                let into = self.node(Slot::Local(f, l.node.binding));
                self.join(v, into);
                if let Some(iv) = self.ranges.interval(&l.node.value, f, known) {
                    known.push(((l.node.binding, Vec::new()), iv));
                    let r = self.eval(&l.node.body, f, known);
                    known.pop();
                    return r;
                }
                self.eval(&l.node.body, f, known)
            }
            MirExpr::Call(c) => match &c.node.callee {
                MirCallee::Fn(target) => self.call(*target, &c.node.args, f, known, line),
                callee => match self.builtin(callee) {
                    Some(name) => self.builtin_call(name, &c.node.args, f, known, line),
                    None => {
                        for a in &c.node.args {
                            let v = self.eval(a, f, known);
                            self.sink(v, || {
                                format!("passed to an intrinsic or a fn value (line {line})")
                            });
                        }
                        Val::Unknown(format!(
                            "the result of an intrinsic or a fn value (line {line})"
                        ))
                    }
                },
            },
            MirExpr::TailCall(tc) => self.call(tc.node.target, &tc.node.args, f, known, line),
            MirExpr::Match(m) => {
                let subject = self.eval(&m.node.subject, f, known);
                let cond = condition(&m.node.subject);
                let out = self.fresh_node();
                for (i, arm) in m.node.arms.iter().enumerate() {
                    match &arm.pattern {
                        MirPattern::Wildcard | MirPattern::Literal(_) => {}
                        MirPattern::Bind(slot, _) => {
                            let into = self.node(Slot::Local(f, *slot));
                            self.join(subject.clone(), into);
                        }
                        MirPattern::EmptyList | MirPattern::Cons { .. } => {
                            self.sink(subject.clone(), || {
                                format!("matched as a list (line {line})")
                            });
                        }
                        _ => {}
                    }
                    match (&arm.pattern, &subject) {
                        // `Result.Ok(x)` of a `Result` the analysis follows.
                        (
                            MirPattern::Ctor {
                                ctor:
                                    crate::ir::mir::expr::MirCtor::Builtin(
                                        crate::ir::hir::BuiltinCtor::ResultOk,
                                    ),
                                bindings,
                                ..
                            },
                            Val::Node(n),
                        ) if bindings.len() == 1 => {
                            let inner = self.payload_of(*n);
                            let into = self.node(Slot::Local(f, bindings[0]));
                            self.union(inner, into);
                        }
                        _ => self.bind_unknown(&arm.pattern, f, line),
                    }
                    let mut narrowed = match (&arm.pattern, &cond) {
                        (MirPattern::Literal(Literal::Bool(b)), Some(c)) => {
                            self.ranges.narrow(c, *b, f, known)
                        }
                        _ => Vec::new(),
                    };
                    narrowed.extend(self.ranges.binds(&m.node.subject, &arm.pattern));
                    narrowed.extend(self.ranges.arm_binds(
                        &m.node.subject,
                        &m.node.arms,
                        i,
                        f,
                        known,
                    ));
                    let depth = known.len();
                    known.extend(narrowed);
                    let v = self.eval(&arm.body, f, known);
                    known.truncate(depth);
                    self.join(v, out);
                }
                Val::Node(out)
            }
            MirExpr::IfThenElse(ite) => {
                let _ = self.eval(&ite.node.cond, f, known);
                let out = self.fresh_node();
                for (branch, b) in [
                    (&ite.node.then_branch, true),
                    (&ite.node.else_branch, false),
                ] {
                    let narrowed = self.ranges.narrow(&ite.node.cond, b, f, known);
                    let depth = known.len();
                    known.extend(narrowed);
                    let v = self.eval(branch, f, known);
                    known.truncate(depth);
                    self.join(v, out);
                }
                Val::Node(out)
            }
            MirExpr::RecordCreate(r) => {
                for field in &r.node.fields {
                    let v = self.eval(&field.value, f, known);
                    let into = self.node(Slot::Field(r.node.type_name.clone(), field.name.clone()));
                    self.join(v, into);
                }
                Val::Fresh
            }
            MirExpr::RecordUpdate(u) => {
                let _ = self.eval(&u.node.base, f, known);
                for field in &u.node.updates {
                    let v = self.eval(&field.value, f, known);
                    let into = self.node(Slot::Field(u.node.type_name.clone(), field.name.clone()));
                    self.join(v, into);
                }
                Val::Fresh
            }
            MirExpr::Return(inner) => {
                let v = self.eval(inner, f, known);
                let into = self.node(Slot::Result(f));
                self.join(v, into);
                Val::Fresh
            }
            MirExpr::FnValue(name) => {
                // A function held as a value has callers the analysis does
                // not see.
                let ids: Vec<FnId> = self
                    .program
                    .fns
                    .values()
                    .filter(|g| &g.name == name)
                    .map(|g| g.fn_id)
                    .collect();
                for id in ids {
                    let g = &self.program.fns[&id];
                    let slots: Vec<Slot> = g
                        .params
                        .iter()
                        .map(|p| Slot::Local(id, p.local))
                        .chain([Slot::Result(id)])
                        .collect();
                    for slot in slots {
                        let n = self.node(slot);
                        self.mark(n, format!("{name} is held as a value (line {line})"));
                    }
                }
                Val::Fresh
            }
            MirExpr::BinOp(b) => {
                self.observe(b.node.lhs.ty());
                self.observe(b.node.rhs.ty());
                for side in [&b.node.lhs, &b.node.rhs] {
                    let v = self.eval(side, f, known);
                    self.sink(v, || format!("an operand of {:?} (line {line})", b.node.op));
                }
                Val::Fresh
            }
            MirExpr::InterpolatedStr(parts) => {
                for p in parts {
                    if let MirStrPart::Expr(ex) = p {
                        self.observe(ex.ty());
                        let v = self.eval(ex, f, known);
                        self.sink(v, || format!("printed (line {line})"));
                    }
                }
                Val::Fresh
            }
            MirExpr::Neg(inner) | MirExpr::Box(inner) | MirExpr::Unbox(inner) => {
                let _ = self.eval(inner, f, known);
                Val::Fresh
            }
            MirExpr::Try(inner) => match self.eval(inner, f, known) {
                Val::Node(n) => Val::Node(self.payload_of(n)),
                Val::Fresh => Val::Fresh,
                Val::Unknown(why) => Val::Unknown(why),
            },
            MirExpr::Construct(c)
                if matches!(
                    c.node.ctor,
                    crate::ir::mir::expr::MirCtor::Builtin(crate::ir::hir::BuiltinCtor::ResultOk)
                ) && c.node.args.len() == 1 =>
            {
                let v = self.eval(&c.node.args[0], f, known);
                let out = self.fresh_node();
                let inner = self.payload_of(out);
                self.join(v, inner);
                Val::Node(out)
            }
            MirExpr::Construct(c) => {
                for a in &c.node.args {
                    let v = self.eval(a, f, known);
                    self.sink(v, || format!("held in a constructor (line {line})"));
                }
                Val::Fresh
            }
            MirExpr::Tuple(items) => {
                for i in items {
                    let v = self.eval(i, f, known);
                    self.sink(v, || format!("held in a tuple (line {line})"));
                }
                Val::Unknown(format!("a tuple (line {line})"))
            }
            MirExpr::MapLiteral(pairs) => {
                for (k, v) in pairs {
                    self.observe(k.ty());
                    for x in [k, v] {
                        let v = self.eval(x, f, known);
                        self.sink(v, || format!("held in a map (line {line})"));
                    }
                }
                Val::Fresh
            }
            MirExpr::IndependentProduct(ip) => {
                for i in &ip.node.items {
                    let v = self.eval(i, f, known);
                    self.sink(v, || {
                        format!("held in an independent product (line {line})")
                    });
                }
                Val::Unknown(format!("an independent product (line {line})"))
            }
            MirExpr::IntWord(_) => Val::Fresh,
        }
    }

    /// A pattern that takes a value apart binds what the analysis does not
    /// follow.
    fn bind_unknown(&mut self, pattern: &MirPattern, f: FnId, line: SourceLine) {
        let mut slots = Vec::new();
        match pattern {
            MirPattern::Wildcard
            | MirPattern::Literal(_)
            | MirPattern::Bind(..)
            | MirPattern::EmptyList => {}
            MirPattern::Cons { head, tail, .. } => slots.extend([*head, *tail]),
            other => pattern_binders(other, &mut slots),
        }
        for slot in slots {
            let n = self.node(Slot::Local(f, slot));
            self.mark(n, format!("bound by a pattern (line {line})"));
        }
    }

    fn call(
        &mut self,
        target: FnId,
        args: &[Spanned<MirExpr>],
        f: FnId,
        known: &mut Known,
        line: SourceLine,
    ) -> Val {
        let Some(g) = self.program.fn_by_id(target) else {
            for a in args {
                let v = self.eval(a, f, known);
                self.sink(v, || {
                    format!("passed to a function outside the program (line {line})")
                });
            }
            return Val::Unknown(format!(
                "the result of a function outside the program (line {line})"
            ));
        };
        // `Bytes.fromList(List.reverse(x))`: the stdlib's validation reads
        // a reversed copy, which an octet group answers as it is.
        if is_bytes_from_list(&g.name)
            && let [arg] = args
            && let Some(list) = self.reversed(arg)
        {
            // `Bytes.fromList(List.reverse(List.take(x, n)))`: the last `n`.
            if let MirExpr::Call(take) = &list.node
                && self.builtin(&take.node.callee) == Some("List.take")
                && let [x, n] = take.node.args.as_slice()
            {
                let _ = self.eval(x, f, known);
                let _ = self.eval(n, f, known);
                return Val::Fresh;
            }
            let _ = self.eval(list, f, known);
            return Val::Fresh;
        }
        let params: Vec<LocalId> = g.params.iter().map(|p| p.local).collect();
        for (i, a) in args.iter().enumerate() {
            let v = self.eval(a, f, known);
            match params.get(i) {
                Some(local) => {
                    let into = self.node(Slot::Local(target, *local));
                    self.join(v, into);
                }
                None => self.sink(v, || format!("an extra argument (line {line})")),
            }
        }
        Val::Node(self.node(Slot::Result(target)))
    }

    fn builtin_call(
        &mut self,
        name: &str,
        args: &[Spanned<MirExpr>],
        f: FnId,
        known: &mut Known,
        line: SourceLine,
    ) -> Val {
        match (name, args) {
            ("List.prepend", [head, tail]) => {
                let h = self.eval(head, f, known);
                self.sink(h, || format!("held in a list (line {line})"));
                let t = self.eval(tail, f, known);
                let out = self.fresh_node();
                self.join(t, out);
                let octet = self
                    .ranges
                    .interval(head, f, known)
                    .is_some_and(|iv| matches!((iv.lo, iv.hi), (Bound::Finite(lo), Bound::Finite(hi)) if lo >= 0 && hi <= 255));
                if !octet {
                    self.mark(
                        out,
                        format!("prepends a value not proven in 0..=255 (line {line})"),
                    );
                }
                Val::Node(out)
            }
            ("List.len", [list]) => {
                let _ = self.eval(list, f, known);
                Val::Fresh
            }
            // A literal of octets reversed is one too.
            ("List.reverse", [list]) if matches!(&list.node, MirExpr::List(items) if items.iter().all(|i| matches!(&i.node, MirExpr::Literal(l) if matches!(l.node, Literal::Int(k) if (0..=255).contains(&k))))) => {
                Val::Fresh
            }
            _ => {
                if !looks_through(name) {
                    for a in args {
                        self.observe(a.ty());
                    }
                }
                for a in args {
                    let v = self.eval(a, f, known);
                    self.sink(v, || format!("passed to {name} (line {line})"));
                }
                Val::Unknown(format!("the result of {name} (line {line})"))
            }
        }
    }

    /// `x` of `List.reverse(x)`.
    fn reversed<'e>(&self, e: &'e Spanned<MirExpr>) -> Option<&'e Spanned<MirExpr>> {
        match &e.node {
            MirExpr::Call(c) if self.builtin(&c.node.callee) == Some("List.reverse") => {
                match c.node.args.as_slice() {
                    [list] => Some(list),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn observe(&mut self, ty: Option<&Type>) {
        if let Some(ty) = ty {
            named_types(ty, &mut |name| {
                self.observed.insert(name.to_string());
            });
        }
    }
}

/// Whether a function is the stdlib's `Bytes.fromList`.
pub fn is_bytes_from_list(name: &str) -> bool {
    matches!(name, "Bytes.fromList" | "Bytes_fromList")
}

/// Builtins that hold or move values of any type without looking at them.
fn looks_through(name: &str) -> bool {
    matches!(
        name,
        "List.prepend"
            | "List.reverse"
            | "List.len"
            | "List.take"
            | "List.drop"
            | "List.concat"
            | "Vector.new"
            | "Vector.get"
            | "Vector.set"
            | "Vector.len"
            | "Vector.fromList"
            | "Option.withDefault"
            | "Result.withDefault"
    )
}

fn record_name(ty: Option<&Type>) -> Option<String> {
    match ty? {
        Type::Named { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn named_types(ty: &Type, out: &mut dyn FnMut(&str)) {
    match ty {
        Type::Named { name, .. } => out(name),
        Type::Result(a, b) | Type::Map(a, b) => {
            named_types(a, out);
            named_types(b, out);
        }
        Type::Option(a) | Type::List(a) | Type::Vector(a) => named_types(a, out),
        Type::Tuple(items) => items.iter().for_each(|t| named_types(t, out)),
        _ => {}
    }
}

fn pattern_binders(p: &MirPattern, out: &mut Vec<LocalId>) {
    match p {
        MirPattern::Bind(slot, _) => out.push(*slot),
        MirPattern::Cons { head, tail, .. } => out.extend([*head, *tail]),
        MirPattern::Tuple(items) => items.iter().for_each(|i| pattern_binders(i, out)),
        MirPattern::Ctor { bindings, .. } => out.extend(bindings.iter().copied()),
        _ => {}
    }
}

/// The comparison a `match` on a `Bool` decides, when its subject is one.
fn condition(subject: &Spanned<MirExpr>) -> Option<&Spanned<MirExpr>> {
    matches!(subject.node, MirExpr::BinOp(_) | MirExpr::Call(_)).then_some(subject)
}

/// Bounds of `Int` expressions: what the representation analysis proved,
/// what a few builtins guarantee, and what an enclosing branch decided.
pub struct Ranges<'p> {
    program: &'p MirProgram,
    bare: &'p BareI64Facts,
    results: std::cell::RefCell<HashMap<FnId, Option<Interval>>>,
}

impl<'p> Ranges<'p> {
    fn new(program: &'p MirProgram, bare: &'p BareI64Facts) -> Self {
        Self {
            program,
            bare,
            results: std::cell::RefCell::new(HashMap::new()),
        }
    }

    fn builtin(&self, callee: &MirCallee) -> Option<&'p str> {
        match callee {
            MirCallee::Builtin(id) => Some(self.program.builtin_name(*id)),
            _ => None,
        }
    }

    /// A bound of `e` in function `f` under `known`, `None` when none is
    /// proven.
    fn interval(&self, e: &Spanned<MirExpr>, f: FnId, known: &Known) -> Option<Interval> {
        let proven = self
            .bare
            .for_fn(f)
            .and_then(|facts| facts.bare_expr_interval(&e.node));
        let narrowed = path_of(&e.node).and_then(|path| {
            known
                .iter()
                .rev()
                .filter(|(p, _)| *p == path)
                .map(|(_, iv)| *iv)
                .reduce(Interval::intersect)
        });
        let own = self.own(e, f, known);
        [proven, narrowed, own]
            .into_iter()
            .flatten()
            .reduce(Interval::intersect)
    }

    fn own(&self, e: &Spanned<MirExpr>, f: FnId, known: &Known) -> Option<Interval> {
        match &e.node {
            MirExpr::Literal(l) => match l.node {
                Literal::Int(k) => Some(Interval::point(i128::from(k))),
                _ => None,
            },
            MirExpr::BinOp(b) => {
                let l = self.interval(&b.node.lhs, f, known)?;
                let r = self.interval(&b.node.rhs, f, known)?;
                match b.node.op {
                    BinOp::Add => Some(l.add(r)),
                    BinOp::Sub => Some(l.sub(r)),
                    BinOp::Mul => Some(l.mul(r)),
                    _ => None,
                }
            }
            MirExpr::Let(l) => self.interval(&l.node.body, f, known),
            MirExpr::Box(inner) | MirExpr::Unbox(inner) => self.interval(inner, f, known),
            MirExpr::Call(c) => match &c.node.callee {
                MirCallee::Fn(target) => self.result(*target),
                // Euclidean: in `0..k` for a positive `k`.
                MirCallee::Intrinsic(crate::ir::hir::BuiltinIntrinsic::IntModEuclid) => {
                    match c.node.args.as_slice() {
                        [_, k] => match &k.node {
                            MirExpr::Literal(l) => match l.node {
                                Literal::Int(k) if k > 0 => {
                                    Some(Interval::between(0, i128::from(k) - 1))
                                }
                                _ => None,
                            },
                            _ => None,
                        },
                        _ => None,
                    }
                }
                callee => {
                    let name = self.builtin(callee)?;
                    let literal = |a: &Spanned<MirExpr>| match &a.node {
                        MirExpr::Literal(l) => match l.node {
                            Literal::Int(k) => Some(i128::from(k)),
                            _ => None,
                        },
                        _ => None,
                    };
                    match (name, c.node.args.as_slice()) {
                        // Euclidean: in `0..k` for a positive `k`.
                        ("Int.mod", [_, k]) => literal(k)
                            .filter(|k| *k > 0)
                            .map(|k| Interval::between(0, k - 1)),
                        // Any `Int` with a mask of no sign bits.
                        ("Bits.and", [a, b]) => literal(b)
                            .or_else(|| literal(a))
                            .filter(|k| *k >= 0)
                            .map(|k| Interval::between(0, k)),
                        ("Option.withDefault", [opt, default]) => {
                            let d = self.interval(default, f, known)?;
                            match &opt.node {
                                MirExpr::Call(g)
                                    if self.builtin(&g.node.callee) == Some("Bytes.get") =>
                                {
                                    Some(Interval::between(0, 255).hull(d))
                                }
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                }
            },
            MirExpr::Match(m) => {
                let mut out: Option<Interval> = None;
                for (i, arm) in m.node.arms.iter().enumerate() {
                    let mut known = known.clone();
                    known.extend(self.binds(&m.node.subject, &arm.pattern));
                    known.extend(self.arm_binds(
                        &m.node.subject,
                        &m.node.arms,
                        i,
                        f,
                        &known.clone(),
                    ));
                    let iv = self.interval(&arm.body, f, &known)?;
                    out = Some(out.map_or(iv, |o| o.hull(iv)));
                }
                out
            }
            MirExpr::IfThenElse(ite) => Some(
                self.interval(&ite.node.then_branch, f, known)?
                    .hull(self.interval(&ite.node.else_branch, f, known)?),
            ),
            _ => None,
        }
    }

    /// What a pattern's binders hold: the octet `Option.Some(x)` of
    /// `Bytes.get` takes is one (the `Bytes` invariant).
    fn binds(&self, subject: &Spanned<MirExpr>, pattern: &MirPattern) -> Known {
        let MirPattern::Ctor {
            ctor: crate::ir::mir::expr::MirCtor::Builtin(crate::ir::hir::BuiltinCtor::OptionSome),
            bindings,
            ..
        } = pattern
        else {
            return Vec::new();
        };
        let octet_read = match &subject.node {
            MirExpr::Call(c) => match &c.node.callee {
                MirCallee::Fn(g) => self
                    .program
                    .fn_by_id(*g)
                    .is_some_and(|g| matches!(g.name.as_str(), "Bytes.get" | "Bytes_get")),
                callee => self.builtin(callee) == Some("Bytes.get"),
            },
            _ => false,
        };
        match (octet_read, bindings.as_slice()) {
            (true, [x]) => vec![((*x, Vec::new()), Interval::between(0, 255))],
            _ => Vec::new(),
        }
    }

    /// What an arm that binds the whole subject (`x -> ...`, or `_`) knows
    /// of it: the subject's bound, less the literals of earlier arms at its
    /// ends (`-1 -> 0; x -> x` over `-1..=255` binds `0..=255`). A subject
    /// that is a read also gets that bound.
    fn arm_binds(
        &self,
        subject: &Spanned<MirExpr>,
        arms: &[crate::ir::mir::expr::MirMatchArm],
        i: usize,
        f: FnId,
        known: &Known,
    ) -> Known {
        let binder = match &arms[i].pattern {
            MirPattern::Bind(slot, _) => Some(*slot),
            MirPattern::Wildcard => None,
            _ => return Vec::new(),
        };
        let Some(mut iv) = self.interval(subject, f, known) else {
            return Vec::new();
        };
        let mut changed = true;
        while changed {
            changed = false;
            for arm in &arms[..i] {
                if let MirPattern::Literal(Literal::Int(k)) = arm.pattern {
                    let k = i128::from(k);
                    if iv.lo == Bound::Finite(k) {
                        iv = iv.intersect(Interval::ge(k + 1));
                        changed = true;
                    } else if iv.hi == Bound::Finite(k) {
                        iv = iv.intersect(Interval::le(k - 1));
                        changed = true;
                    }
                }
            }
        }
        let mut out: Known = binder
            .map(|slot| ((slot, Vec::new()), iv))
            .into_iter()
            .collect();
        if let Some(path) = path_of(&subject.node) {
            out.push((path, iv));
        }
        out
    }

    /// A bound of what `target` returns, from its body alone; recursion
    /// gives none.
    fn result(&self, target: FnId) -> Option<Interval> {
        if let Some(known) = self.results.borrow().get(&target) {
            return *known;
        }
        self.results.borrow_mut().insert(target, None);
        let g = self.program.fn_by_id(target)?;
        let iv = self.interval(&g.body, target, &Vec::new());
        self.results.borrow_mut().insert(target, iv);
        iv
    }

    /// What taking branch `taken` of `cond` proves.
    fn narrow(&self, cond: &Spanned<MirExpr>, taken: bool, f: FnId, known: &Known) -> Known {
        let mut out = Vec::new();
        match &cond.node {
            MirExpr::BinOp(b) => {
                let (op, lhs, rhs) = (b.node.op, &b.node.lhs, &b.node.rhs);
                let k =
                    |e: &Spanned<MirExpr>| self.interval(e, f, known).and_then(Interval::as_point);
                // `x op k`, or `k op x` turned around.
                let (path, op, k) = match (path_of(&lhs.node), k(rhs), path_of(&rhs.node), k(lhs)) {
                    (Some(p), Some(k), _, _) => (p, op, k),
                    (_, _, Some(p), Some(k)) => (p, flip(op), k),
                    _ => return out,
                };
                let op = if taken { Some(op) } else { negate(op) };
                let iv = match op {
                    Some(BinOp::Lt) => Interval::le(k - 1),
                    Some(BinOp::Lte) => Interval::le(k),
                    Some(BinOp::Gt) => Interval::ge(k + 1),
                    Some(BinOp::Gte) => Interval::ge(k),
                    Some(BinOp::Eq) => Interval::point(k),
                    _ => return out,
                };
                out.push((path, iv));
            }
            MirExpr::Call(c) if taken && self.builtin(&c.node.callee) == Some("Bool.and") => {
                for a in &c.node.args {
                    out.extend(self.narrow(a, true, f, known));
                }
            }
            MirExpr::Call(c) if !taken && self.builtin(&c.node.callee) == Some("Bool.or") => {
                for a in &c.node.args {
                    out.extend(self.narrow(a, false, f, known));
                }
            }
            _ => {}
        }
        out
    }
}

fn flip(op: BinOp) -> BinOp {
    match op {
        BinOp::Lt => BinOp::Gt,
        BinOp::Lte => BinOp::Gte,
        BinOp::Gt => BinOp::Lt,
        BinOp::Gte => BinOp::Lte,
        other => other,
    }
}

fn negate(op: BinOp) -> Option<BinOp> {
    match op {
        BinOp::Lt => Some(BinOp::Gte),
        BinOp::Lte => Some(BinOp::Gt),
        BinOp::Gt => Some(BinOp::Lte),
        BinOp::Gte => Some(BinOp::Lt),
        _ => None,
    }
}

/// The local and fields a read reaches, when it is one.
fn path_of(e: &MirExpr) -> Option<Path> {
    match e {
        MirExpr::Local(l) => Some((l.node.slot, Vec::new())),
        MirExpr::Project(p) => {
            let (slot, mut fields) = path_of(&p.node.base.node)?;
            fields.push(p.node.field.clone());
            Some((slot, fields))
        }
        MirExpr::Box(inner) | MirExpr::Unbox(inner) => path_of(&inner.node),
        _ => None,
    }
}

/// Rewrite the octet groups of `program` for a backend that keeps them as
/// byte buffers: `List.prepend(h, x)` onto a list of a group becomes
/// `__oct_push(h, x)`, a literal list flowing into a group becomes pushes
/// onto `[]`, and `Bytes.fromList(List.reverse(x))` of one becomes
/// `Result.Ok(__oct_bytes(x))`. `[]` stays as it is, and so does every
/// other read (`List.len` included). Whether anything was rewritten.
pub fn rewrite(program: &mut MirProgram, facts: &OctetFacts) -> bool {
    if facts.is_empty() {
        return false;
    }
    let fns: HashMap<FnId, (String, Vec<LocalId>)> = program
        .fns
        .iter()
        .map(|(id, f)| {
            (
                *id,
                (f.name.clone(), f.params.iter().map(|p| p.local).collect()),
            )
        })
        .collect();
    let ctx = Rewrite {
        facts,
        builtins: program.builtins.clone(),
        fns,
    };
    let mut any = false;
    for (id, f) in program.fns.iter_mut() {
        let octet = facts.result(*id);
        ctx.rw(&mut f.body, *id, octet, &mut any);
    }
    any
}

struct Rewrite<'a> {
    facts: &'a OctetFacts,
    builtins: Vec<String>,
    fns: HashMap<FnId, (String, Vec<LocalId>)>,
}

fn is_int_list(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::List(inner)) if **inner == Type::Int)
}

fn intrinsic(
    which: crate::ir::hir::BuiltinIntrinsic,
    args: Vec<Spanned<MirExpr>>,
    line: SourceLine,
    ty: Option<&Type>,
) -> Spanned<MirExpr> {
    let call = Spanned::new(
        MirExpr::Call(Spanned::new(
            crate::ir::mir::expr::MirCall {
                callee: MirCallee::Intrinsic(which),
                args,
            },
            line,
        )),
        line,
    );
    if let Some(ty) = ty {
        call.set_ty(ty.clone());
    }
    call
}

impl Rewrite<'_> {
    fn builtin(&self, callee: &MirCallee) -> Option<&str> {
        match callee {
            MirCallee::Builtin(id) => self.builtins.get(id.0 as usize).map(String::as_str),
            _ => None,
        }
    }

    /// Whether `e` gives a value of an octet group.
    fn is_octet(&self, e: &Spanned<MirExpr>, f: FnId) -> bool {
        match &e.node {
            MirExpr::Local(l) => self.facts.local(f, l.node.slot),
            MirExpr::Project(p) => record_name(p.node.base.ty())
                .is_some_and(|record| self.facts.field(&record, &p.node.field)),
            MirExpr::Call(c) => match &c.node.callee {
                MirCallee::Fn(g) => self.facts.result(*g),
                MirCallee::Intrinsic(crate::ir::hir::BuiltinIntrinsic::OctPush) => true,
                callee => match (self.builtin(callee), c.node.args.as_slice()) {
                    (Some("List.prepend"), [_, tail]) => self.is_octet(tail, f),
                    _ => false,
                },
            },
            MirExpr::TailCall(tc) => self.facts.result(tc.node.target),
            // What a function's `Result.Ok` held.
            MirExpr::Try(inner) => match &inner.node {
                MirExpr::Call(c) => match c.node.callee {
                    MirCallee::Fn(g) => self.facts.result_ok(g),
                    _ => false,
                },
                _ => false,
            },
            MirExpr::Let(l) => self.is_octet(&l.node.body, f),
            MirExpr::Match(m) => m.node.arms.iter().any(|arm| self.is_octet(&arm.body, f)),
            MirExpr::IfThenElse(ite) => {
                self.is_octet(&ite.node.then_branch, f) || self.is_octet(&ite.node.else_branch, f)
            }
            _ => false,
        }
    }

    /// Rewrite `e`, whose value flows into an octet group when `octet`.
    fn rw(&self, e: &mut Spanned<MirExpr>, f: FnId, octet: bool, any: &mut bool) {
        let line = e.line;
        let ty = e.ty().cloned();
        let int_list = is_int_list(ty.as_ref());
        match &mut e.node {
            MirExpr::List(items) if octet && !items.is_empty() && int_list => {
                // `[a, b, c]` is `a` in front of `b` in front of `c`: pushed
                // `c` first.
                let empty = Spanned::new(MirExpr::List(Vec::new()), line);
                if let Some(ty) = &ty {
                    empty.set_ty(ty.clone());
                }
                let mut acc = empty;
                for item in std::mem::take(items).into_iter().rev() {
                    acc = intrinsic(
                        crate::ir::hir::BuiltinIntrinsic::OctPush,
                        vec![item, acc],
                        line,
                        ty.as_ref(),
                    );
                }
                *e = acc;
                *any = true;
            }
            MirExpr::Call(c) => match &c.node.callee {
                MirCallee::Fn(g) => {
                    let g = *g;
                    let (name, params) = self.fns.get(&g).cloned().unwrap_or_default();
                    if is_bytes_from_list(&name)
                        && let [arg] = c.node.args.as_mut_slice()
                        && let MirExpr::Call(rev) = &mut arg.node
                        && self.builtin(&rev.node.callee) == Some("List.reverse")
                        && let [inner] = rev.node.args.as_mut_slice()
                    {
                        let bytes_ty = match &ty {
                            Some(Type::Result(ok, _)) => Some((**ok).clone()),
                            _ => None,
                        };
                        let take = match &inner.node {
                            MirExpr::Call(take) => {
                                self.builtin(&take.node.callee) == Some("List.take")
                            }
                            _ => false,
                        };
                        let whole = !take && is_int_list(inner.ty()) && self.is_octet(inner, f);
                        let made = match &mut inner.node {
                            _ if whole => None,
                            MirExpr::Call(t) if take => match t.node.args.as_mut_slice() {
                                [x, n] if is_int_list(x.ty()) && self.is_octet(x, f) => {
                                    self.rw(x, f, true, any);
                                    self.rw(n, f, false, any);
                                    Some(intrinsic(
                                        crate::ir::hir::BuiltinIntrinsic::OctLast,
                                        vec![x.clone(), n.clone()],
                                        line,
                                        bytes_ty.as_ref(),
                                    ))
                                }
                                _ => None,
                            },
                            _ => None,
                        };
                        let made = match whole {
                            true => {
                                self.rw(inner, f, true, any);
                                Some(intrinsic(
                                    crate::ir::hir::BuiltinIntrinsic::OctBytes,
                                    vec![inner.clone()],
                                    line,
                                    bytes_ty.as_ref(),
                                ))
                            }
                            false => made,
                        };
                        if let Some(bytes) = made {
                            let ok = Spanned::new(
                                MirExpr::Construct(Spanned::new(
                                    crate::ir::mir::expr::MirConstruct {
                                        ctor: crate::ir::mir::expr::MirCtor::Builtin(
                                            crate::ir::hir::BuiltinCtor::ResultOk,
                                        ),
                                        args: vec![bytes],
                                    },
                                    line,
                                )),
                                line,
                            );
                            if let Some(ty) = &ty {
                                ok.set_ty(ty.clone());
                            }
                            *e = ok;
                            *any = true;
                            return;
                        }
                    }
                    for (i, a) in c.node.args.iter_mut().enumerate() {
                        let into = params.get(i).is_some_and(|p| self.facts.local(g, *p));
                        self.rw(a, f, into, any);
                    }
                }
                callee => {
                    let prepend =
                        self.builtin(callee) == Some("List.prepend") && c.node.args.len() == 2;
                    if prepend {
                        let into = int_list && (octet || self.is_octet(&c.node.args[1], f));
                        self.rw(&mut c.node.args[0], f, false, any);
                        self.rw(&mut c.node.args[1], f, into, any);
                        if into {
                            c.node.callee =
                                MirCallee::Intrinsic(crate::ir::hir::BuiltinIntrinsic::OctPush);
                            *any = true;
                        }
                        return;
                    }
                    for a in c.node.args.iter_mut() {
                        self.rw(a, f, false, any);
                    }
                }
            },
            MirExpr::TailCall(tc) => {
                let g = tc.node.target;
                let params = self.fns.get(&g).map(|(_, p)| p.clone()).unwrap_or_default();
                for (i, a) in tc.node.args.iter_mut().enumerate() {
                    let into = params.get(i).is_some_and(|p| self.facts.local(g, *p));
                    self.rw(a, f, into, any);
                }
            }
            MirExpr::Let(l) => {
                let into = self.facts.local(f, l.node.binding);
                self.rw(&mut l.node.value, f, into, any);
                self.rw(&mut l.node.body, f, octet, any);
            }
            MirExpr::Match(m) => {
                let into = m.node.arms.iter().any(|arm| {
                    matches!(arm.pattern, MirPattern::Bind(slot, _) if self.facts.local(f, slot))
                });
                self.rw(&mut m.node.subject, f, into, any);
                for arm in m.node.arms.iter_mut() {
                    self.rw(&mut arm.body, f, octet, any);
                }
            }
            MirExpr::IfThenElse(ite) => {
                self.rw(&mut ite.node.cond, f, false, any);
                self.rw(&mut ite.node.then_branch, f, octet, any);
                self.rw(&mut ite.node.else_branch, f, octet, any);
            }
            MirExpr::RecordCreate(r) => {
                let record = r.node.type_name.clone();
                for field in r.node.fields.iter_mut() {
                    let into = self.facts.field(&record, &field.name);
                    self.rw(&mut field.value, f, into, any);
                }
            }
            MirExpr::RecordUpdate(u) => {
                let record = u.node.type_name.clone();
                self.rw(&mut u.node.base, f, false, any);
                for field in u.node.updates.iter_mut() {
                    let into = self.facts.field(&record, &field.name);
                    self.rw(&mut field.value, f, into, any);
                }
            }
            MirExpr::Return(inner) => {
                let into = self.facts.result(f);
                self.rw(inner, f, into, any);
            }
            node => crate::ir::mir::expr::walk_children_mut(node, &mut |child| {
                self.rw(child, f, false, any)
            }),
        }
    }
}
