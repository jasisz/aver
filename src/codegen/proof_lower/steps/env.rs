//! What a producer may consult: source definitions it is allowed to open,
//! cited laws, and the hypotheses in scope.

use std::collections::HashMap;

use crate::ast::Stmt;
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{Term, canon, canon_ref};
use crate::ir::proof_steps::{Const, Def, Eqn, LawRef, Proof};

/// A divisor, the hypotheses in scope when it was read, and its proof of
/// `k > 0` if there is one ([`Env::divisor_positive`]).
pub(crate) type DivisorProof = (Term, Vec<(String, Eqn)>, Option<Proof>);

pub(crate) struct Env<'a> {
    pub inputs: &'a ProofLowerInputs<'a>,
    defs: HashMap<FnId, Option<Def>>,
    /// Every pure definition as written, before the termination gate.
    raw_defs: std::cell::RefCell<HashMap<FnId, Option<Def>>>,
    /// Whether a definition counts an Int toward zero, and whether it
    /// divides it on the way, by [`Env::counts_down`].
    countdowns: std::cell::RefCell<HashMap<FnId, (bool, bool)>>,
    /// The division by a literal that a step takes out the multiples of
    /// the divisor from, with its proof, which reads no hypothesis, by the
    /// division (see `Env::division_step`).
    pub(crate) wholes:
        std::cell::RefCell<HashMap<String, Option<(crate::ir::proof_steps::Proof, Term)>>>,
    /// Definitions opened so far, in first-use order.
    pub used: Vec<FnId>,
    consts: HashMap<String, Option<Const>>,
    /// Module-level bindings opened so far, in first-use order.
    pub used_consts: Vec<String>,
    pub laws: Vec<LawRef>,
    /// Laws the author cited with `using`, which evaluation may apply left
    /// to right where it stops.
    pub rewrite_laws: Vec<LawRef>,
    /// Every law the author cited, also those that would loop as rewrite
    /// rules: one instance may still prove an equation outright.
    pub cited_all: Vec<LawRef>,
    /// The builtin facts evaluation rewrites with where it stops and no
    /// cited law applies, cited or not
    /// ([`crate::ir::proof_steps::facts::NORMALIZING`]).
    pub fact_rules: Vec<LawRef>,
    /// The laws before this one it does not cite, noted in `hints` where
    /// evaluation stops (never applied).
    pub hint_laws: Vec<LawRef>,
    /// For a law without a `using` line, on the second pass only: the
    /// earlier laws the steps closed, which evaluation may apply left to
    /// right where it stops when neither a cited law nor a builtin fact
    /// does (see `super::auto_pool`).
    pub auto_laws: Vec<LawRef>,
    pub hyps: Vec<(String, Eqn)>,
    /// The last [`Env::opened_views`], with the hypotheses it read and the
    /// definitions it looked up, in order.
    pub(crate) views_memo: Option<Box<ViewsMemo>>,
    /// Inside a case of a split on a countdown's guard (see
    /// `Env::countdown_guard`): another countdown splits there only on a
    /// guard linear arithmetic decides, so one case closes at once.
    pub countdown_split: bool,
    /// The module of the law being proved; `None` for the entry module.
    pub law_scope: Option<String>,
    /// Read definitions as written, `?` left as it stands (see
    /// `by_plan::goal`).
    pub as_written: bool,
    pub fuel: usize,
    /// Fuel for evaluating hypotheses to their normal form
    /// ([`Env::normal_fact`]), apart from `fuel`, so stating hypotheses
    /// the way the case has evaluated them neither starves the claim's own
    /// evaluation nor runs without bound.
    pub normal_fuel: usize,
    next_hyp: usize,
    /// Induction hypotheses named so far, so names stay distinct.
    pub next_ih: usize,
    /// Evaluations in progress, one inside another.
    pub nesting: usize,
    /// The keys of the builtin facts and earlier laws whose left side
    /// matches a part of a term evaluation stopped at: what a hinted
    /// `using` line may add (see `super::using_hint`).
    pub hints: Vec<String>,
    /// Where evaluation stopped: both sides and the hypotheses in scope,
    /// [`MAX_STOPS`] at most, for the hint naming a missing lemma
    /// (`super::lemma`).
    pub stops: Vec<Stop>,
    /// Advice for a law the steps closed: the `induction` line that would
    /// have spared trying the others.
    pub advice: Vec<String>,
    /// The functions an induction in progress follows, innermost last.
    pub inducting: Vec<crate::ir::identity::FnId>,
    /// Where a cited law matched but a conjunct of its `when` found no
    /// proof, for the refusal.
    pub open_premises: Vec<String>,
    /// Predicate hypotheses opened on the way to the current case, so
    /// none is opened twice.
    pub opened: Vec<Term>,
    /// Predicate hypotheses whose evaluation gave nothing new in this
    /// attempt, so none is evaluated again.
    pub barren: Vec<Term>,
    /// Whether the `when` of a cited law is being proved, so that a cited
    /// law with a `when` is not offered again inside it.
    pub proving_cited_when: bool,
    /// Whether a definition that divides an Int down to zero may be
    /// opened; an attempt that opened one and failed is tried again
    /// without, so the call stays whole as before.
    pub open_halving: bool,
    /// Whether such a definition was opened.
    pub met_halving: std::cell::Cell<bool>,
    /// The obligation's givens, which a fresh name must avoid.
    pub givens: Vec<String>,
    /// The givens of a finite type, which a split into every value covers.
    pub finite: Vec<String>,
    /// The names a constructor split has bound so far: a term that
    /// mentions one is not split on again (see [`super::split`]).
    pub split_binders: Vec<String>,
    /// Evaluations remembered for this obligation, shared by its
    /// attempts ([`super::memo`]); `None` with the memo off.
    pub memo: Option<std::rc::Rc<std::cell::RefCell<super::memo::Memo>>>,
    pub memo_mode: super::memo::Mode,
    /// What the evaluation being remembered has written so far.
    pub trace: Option<super::memo::Trace>,
    /// Whether the fuel ran out, so that what was found depends on how
    /// much there was.
    pub ran_dry: bool,
    /// Steps left for reading the hypotheses of a case again
    /// ([`super::hypotheses`]).
    pub refute_fuel: usize,
    /// The hypotheses (hashed) of the cases read again so far.
    pub refuted: Vec<u64>,
    /// The one open line of a cited law's `when` where the law matched and
    /// every other line was proved, in the order met: what a split on a
    /// cited `when` may take up ([`super::split_on_cited_when`]).
    pub open_whens: Vec<Term>,
    /// Whether a premise is being proved by evaluation
    /// ([`Env::discharge`]), so the laws met inside are not proved so again.
    pub discharging: bool,
    /// Whether a divisor that is not a literal is being shown positive for
    /// linear arithmetic ([`Env::linear_proof`]), so a division inside it
    /// is not read again.
    pub naming_divisor: bool,
    /// What [`Env::divisor_positive`] found, by divisor and the hypotheses
    /// in scope then.
    pub divisors: Vec<DivisorProof>,
    /// The comparisons (the opposite of the goal first) whose products
    /// settled nothing ([`super::nonlinear`]), so they are not multiplied
    /// out again.
    pub unsettled_products: Vec<Vec<Eqn>>,
    /// Whether linear arithmetic also reads the comparisons inside a true
    /// conjunction and that lengths are not negative where the facts in
    /// scope do not settle a comparison: only while a case is read for a
    /// contradiction, a premise is proved by evaluation, or a cited `when`
    /// is split on ([`Env::linear_proof`]), so the evaluation everywhere
    /// else pays nothing for it.
    pub wide_linear: bool,
    /// Steps left for evaluating the hypotheses of a case the way its claim
    /// was evaluated ([`super::evaluated`]), apart from every other budget.
    pub read_fuel: usize,
    /// How many evaluated forms were named so far, so their names stay
    /// distinct.
    pub next_read: usize,
    /// Whether a case is tried only as its hypotheses stand: set while an
    /// induction may still state its claim at other values, so the proof it
    /// finds that way comes first.
    pub hypotheses_as_stated: bool,
    /// Steps left for evaluating a claim again under its evaluated
    /// hypotheses.
    pub read_claim_fuel: usize,
    /// The cases (their hypotheses and claim, hashed) whose hypotheses were
    /// evaluated so far.
    pub read_seen: Vec<u64>,
    /// How many such readings are in progress, one inside the claim another
    /// went on to.
    pub reading: usize,
    /// Whether a table of equal terms is being built ([`super::egraph`]),
    /// so none is built inside it.
    pub in_table: bool,
    /// How many tables this attempt has built.
    pub table_runs: usize,
    /// How many hypothesis names the tables' explanations have given out.
    pub table_names: usize,
    /// How many evaluations of a claim are in progress, one inside another
    /// (see `Env::prove_by_evaluation`).
    pub evaluating: usize,
    /// How many list splits of tables are in progress (see
    /// [`super::egraph`]).
    pub table_splits: usize,
}

/// Whether `t` builds or updates a record of a refined type anywhere, in a
/// match arm too.
fn builds_refined(t: &Term, inputs: &ProofLowerInputs) -> bool {
    use crate::ir::hir::ResolvedExpr;
    match &t.node {
        ResolvedExpr::RecordCreate { type_name, .. }
        | ResolvedExpr::RecordUpdate { type_name, .. } => {
            let bare = type_name.rsplit('.').next().unwrap_or(type_name);
            if crate::codegen::common::refinement_info_for(bare, inputs).is_some() {
                return true;
            }
        }
        ResolvedExpr::Match { arms, .. }
            if arms.iter().any(|a| builds_refined(&a.body, inputs)) =>
        {
            return true;
        }
        _ => {}
    }
    crate::ir::proof_steps::term::children(t)
        .into_iter()
        .any(|c| builds_refined(c, inputs))
}

/// Whether `id` sits in a group of definitions that call each other (a
/// cycle through another definition) in which some member passes two or
/// more of its own parameters of a list or named type, as they stand, to
/// the parameter another member chooses by.
fn fuelled_group(id: FnId, ds: &[Def]) -> bool {
    use crate::ast::Type;
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    use crate::ir::proof_steps::induct::def_callees;
    // The members: the reachable definitions that reach `id` back.
    let reaches_back = |d: &Def| -> bool {
        let mut seen = vec![d.fn_id];
        let mut todo = vec![d.fn_id];
        while let Some(at) = todo.pop() {
            let Some(def) = ds.iter().find(|x| x.fn_id == at) else {
                continue;
            };
            for c in def_callees(def) {
                if c == id {
                    return true;
                }
                if !seen.contains(&c) {
                    seen.push(c);
                    todo.push(c);
                }
            }
        }
        false
    };
    let group: Vec<&Def> = ds.iter().filter(|d| reaches_back(d)).collect();
    if !group.iter().any(|d| d.fn_id != id) {
        return false;
    }
    // The parameter each member chooses by with its `match`: the one a
    // measure of the group would count for it.
    let measured: Vec<(FnId, usize)> = group
        .iter()
        .filter_map(|d| {
            let ResolvedExpr::Match { subject, .. } = &d.body.node else {
                return None;
            };
            let ResolvedExpr::Ident(n) = &subject.node else {
                return None;
            };
            d.params.iter().position(|p| p == n).map(|i| (d.fn_id, i))
        })
        .collect();
    fn handed(t: &Term, measured: &[(FnId, usize)], out: &mut Vec<String>) {
        if let ResolvedExpr::Call(ResolvedCallee::Fn(f), args) = &t.node {
            for (i, a) in args.iter().enumerate() {
                if measured.contains(&(*f, i))
                    && let ResolvedExpr::Ident(n) = &a.node
                    && !out.contains(n)
                {
                    out.push(n.clone());
                }
            }
        }
        for c in crate::ir::proof_steps::term::children(t) {
            handed(c, measured, out);
        }
        if let ResolvedExpr::Match { arms, .. } = &t.node {
            for arm in arms.iter() {
                handed(&arm.body, measured, out);
            }
        }
    }
    group.iter().any(|d| {
        let mut names = Vec::new();
        handed(&d.body, &measured, &mut names);
        d.params
            .iter()
            .zip(&d.param_types)
            .filter(|(p, ty)| names.contains(p) && matches!(ty, Type::List(_) | Type::Named { .. }))
            .count()
            >= 2
    })
}

/// What [`Env::opened_views`] gave for the hypotheses `hyps`.
pub(crate) struct ViewsMemo {
    pub hyps: Vec<(String, Eqn)>,
    pub looked_up: Vec<FnId>,
    pub views: std::rc::Rc<Vec<(Term, crate::ir::proof_steps::Proof, FnId)>>,
}

/// Whether two lists of hypotheses are the same, names and terms part for
/// part ([`crate::ir::proof_steps::term::exact_eq`]).
pub(crate) fn same_hyps(a: &[(String, Eqn)], b: &[(String, Eqn)]) -> bool {
    use crate::ir::proof_steps::term::exact_eq;
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|((n, e), (m, f))| n == m && exact_eq(&e.lhs, &f.lhs) && exact_eq(&e.rhs, &f.rhs))
}

/// Where evaluation stopped: the two sides it could not bring together,
/// and the hypotheses in scope there.
#[derive(Clone)]
pub(crate) struct Stop {
    pub lhs: Term,
    pub rhs: Term,
    pub hyps: Vec<Eqn>,
}

/// Most stops one attempt keeps.
pub(crate) const MAX_STOPS: usize = 16;

/// The evaluation steps one attempt on an obligation may take.
pub(crate) const FUEL: usize = 4000;

impl<'a> Env<'a> {
    pub(crate) fn new(inputs: &'a ProofLowerInputs<'a>) -> Self {
        Self {
            inputs,
            defs: HashMap::new(),
            raw_defs: std::cell::RefCell::new(HashMap::new()),
            countdowns: std::cell::RefCell::new(HashMap::new()),
            wholes: std::cell::RefCell::new(HashMap::new()),
            used: Vec::new(),
            consts: HashMap::new(),
            used_consts: Vec::new(),
            laws: Vec::new(),
            rewrite_laws: Vec::new(),
            cited_all: Vec::new(),
            fact_rules: Vec::new(),
            hint_laws: Vec::new(),
            auto_laws: Vec::new(),
            hyps: Vec::new(),
            views_memo: None,
            countdown_split: false,
            law_scope: None,
            as_written: false,
            fuel: FUEL,
            normal_fuel: 2000,
            next_hyp: 0,
            next_ih: 0,
            nesting: 0,
            hints: Vec::new(),
            stops: Vec::new(),
            advice: Vec::new(),
            inducting: Vec::new(),
            open_premises: Vec::new(),
            opened: Vec::new(),
            barren: Vec::new(),
            proving_cited_when: false,
            open_halving: true,
            met_halving: std::cell::Cell::new(false),
            givens: Vec::new(),
            finite: Vec::new(),
            split_binders: Vec::new(),
            memo: None,
            memo_mode: super::memo::Mode::Off,
            trace: None,
            ran_dry: false,
            refute_fuel: super::hypotheses::REFUTE_FUEL,
            refuted: Vec::new(),
            open_whens: Vec::new(),
            naming_divisor: false,
            divisors: Vec::new(),
            unsettled_products: Vec::new(),
            discharging: false,
            wide_linear: false,
            read_fuel: super::evaluated::budget(super::evaluated::READ_FUEL),
            read_claim_fuel: super::evaluated::budget(super::evaluated::READ_CLAIM_FUEL),
            next_read: 0,
            hypotheses_as_stated: false,
            read_seen: Vec::new(),
            reading: 0,
            in_table: false,
            table_runs: 0,
            table_names: 0,
            evaluating: 0,
            table_splits: 0,
        }
    }

    /// Note a write for the evaluation being remembered, if any.
    pub(crate) fn traced(&mut self, e: super::memo::Effect) {
        if let Some(trace) = &mut self.trace {
            trace.push(e);
        }
    }

    /// Carry law `law` in the script, once.
    pub(crate) fn use_law(&mut self, law: LawRef) {
        if self.trace.is_some() {
            self.traced(super::memo::Effect::Law(Box::new(law.clone())));
        }
        if !self.laws.iter().any(|l| l.key == law.key) {
            self.laws.push(law);
        }
    }

    /// How many hypothesis names [`Self::fresh_hyp`] has given out.
    pub(crate) fn next_hyp_count(&self) -> usize {
        self.next_hyp
    }

    /// Give the names from `count` on out again: what named them was
    /// dropped, so nothing in scope or in a proof holds one.
    pub(crate) fn rewind_hyp_count(&mut self, count: usize) {
        self.next_hyp = count;
    }

    pub(crate) fn fresh_hyp(&mut self) -> String {
        self.next_hyp += 1;
        format!("h_steps{}", self.next_hyp)
    }

    pub(crate) fn burn(&mut self) -> Result<(), String> {
        if self.fuel == 0 {
            self.ran_dry = true;
            return Err("the producer ran out of fuel".into());
        }
        self.fuel -= 1;
        Ok(())
    }

    /// A definition steps may open: pure, local bindings then one
    /// expression, and, if it calls itself, through the termination gate of
    /// [`crate::ir::proof_steps::induct::recursion`].
    pub(crate) fn def(&mut self, id: FnId) -> Option<Def> {
        self.traced(super::memo::Effect::Def(id));
        if let Some(found) = self.defs.get(&id) {
            return found.clone();
        }
        let built = self.build_def(id);
        self.defs.insert(id, built.clone());
        built
    }

    /// [`Self::def`] without the copy: the same lookup, read in place.
    pub(crate) fn def_ref(&mut self, id: FnId) -> Option<&Def> {
        self.traced(super::memo::Effect::Def(id));
        if !self.defs.contains_key(&id) {
            let built = self.build_def(id);
            self.defs.insert(id, built);
        }
        self.defs.get(&id).and_then(Option::as_ref)
    }

    pub(crate) fn mark_used(&mut self, id: FnId) {
        self.traced(super::memo::Effect::Used(id));
        if !self.used.contains(&id) {
            self.used.push(id);
        }
    }

    /// The definitions opened so far. Each was looked up when it was
    /// opened, so reading them back is no lookup an evaluation makes
    /// (see [`super::memo`]): it depends on what was opened before.
    pub(crate) fn used_defs(&mut self) -> Vec<Def> {
        let trace = self.trace.take();
        let ids = self.used.clone();
        let defs = ids.into_iter().filter_map(|id| self.def(id)).collect();
        self.trace = trace;
        defs
    }

    /// A module-level binding steps may open, by the name terms read it
    /// under (see [`qualify_bindings`]).
    pub(crate) fn constant(&mut self, name: &str) -> Option<Const> {
        if let Some(found) = self.consts.get(name) {
            return found.clone();
        }
        let built = self.build_const(name);
        self.consts.insert(name.to_string(), built.clone());
        built
    }

    pub(crate) fn mark_const_used(&mut self, name: &str) {
        if self.trace.is_some() {
            self.traced(super::memo::Effect::Const(name.to_string()));
        }
        if !self.used_consts.iter().any(|n| n == name) {
            self.used_consts.push(name.to_string());
        }
    }

    pub(crate) fn used_consts(&mut self) -> Vec<Const> {
        let names = self.used_consts.clone();
        names.iter().filter_map(|n| self.constant(n)).collect()
    }

    fn build_const(&self, name: &str) -> Option<Const> {
        // A name without a dot is a local: a given, a parameter or a name a
        // pattern binds, whatever module-level binding is spelled like it.
        let (prefix, short) = name.rsplit_once('.')?;
        let scope = (!prefix.is_empty()).then_some(prefix);
        let binding = crate::codegen::proof_lower::module_bindings_of(self.inputs, scope)
            .into_iter()
            .find(|b| b.name == short)?;
        let ty = binding.declared_type().ok()?;
        let value = canon(&self.inputs.resolve_expr(&binding.value, scope));
        Some(Const {
            name: name.to_string(),
            ty: super::finite::qualify(self.inputs, &ty, scope),
            value: qualify_bindings(&value, self.inputs, scope, &[]),
        })
    }

    /// The definitions reachable from `id` through calls, `id`'s first:
    /// what a recursion through other definitions reads.
    pub(crate) fn reachable_defs(&self, id: FnId) -> Vec<Def> {
        let mut out: Vec<Def> = Vec::new();
        let mut todo = vec![id];
        while let Some(at) = todo.pop() {
            if out.iter().any(|d| d.fn_id == at) {
                continue;
            }
            let Some(d) = self.raw_def(at) else {
                continue;
            };
            let called = crate::ir::proof_steps::induct::def_callees(&d);
            out.push(d);
            todo.extend(called.into_iter().rev());
        }
        out
    }

    /// Whether `def` counts an Int toward zero, read through the helpers
    /// it calls back through, and whether some recursive call divides it.
    pub(crate) fn counts_down(&self, def: &Def) -> (bool, bool) {
        use crate::ir::proof_steps::induct;
        if let Some(found) = self.countdowns.borrow().get(&def.fn_id) {
            return *found;
        }
        let ds = self.reachable_defs(def.fn_id);
        let countdown = matches!(
            induct::recursion(def, &ds),
            Ok(Some(induct::Recursion { guard: Some(_), .. }))
        );
        let found = (countdown, induct::divides_down(def, &ds));
        self.countdowns.borrow_mut().insert(def.fn_id, found);
        found
    }

    /// A pure definition as written, before the termination gate.
    pub(crate) fn raw_def(&self, id: FnId) -> Option<Def> {
        if let Some(found) = self.raw_defs.borrow().get(&id) {
            return found.clone();
        }
        let built = self.read_def(id);
        self.raw_defs.borrow_mut().insert(id, built.clone());
        built
    }

    fn build_def(&self, id: FnId) -> Option<Def> {
        let def = self.raw_def(id)?;
        // A definition of another module that builds a record of a refined
        // type, which that module exposes opaque: the law's module cannot see
        // inside such a record, and Lean states it as a `Subtype` that step
        // terms do not spell, so steps leave the definition closed.
        let own = self.inputs.symbol_table.fn_entry(id).key.scope_str();
        let terms = || def.lets.iter().map(|(_, v)| v).chain([&def.body]);
        if own != self.law_scope.as_deref() && terms().any(|t| builds_refined(t, self.inputs)) {
            return None;
        }
        // A recursive function opens only when its own recursion passes the
        // gate, a descent into a part or a division of an Int down to zero,
        // read through the helpers it calls back through; or when every
        // cycle through it passes one that does.
        if self.inputs.recursive_fns.contains(&id) {
            use crate::ir::proof_steps::induct;
            let ds = self.reachable_defs(id);
            if !matches!(induct::recursion(&def, &ds), Ok(Some(_))) && !induct::rooted(&def, &ds) {
                return None;
            }
            // A group of definitions calling each other where one member
            // hands two of its own parameters of a recursive type into the
            // group (`collect(l, v, r)` calling `values(l)` and `values(r)`):
            // the Lean export measures such a group by no single parameter
            // and writes it with fuel, where opening a member at an arm is
            // no definitional step. Steps leave it closed, so the kernel
            // never accepts what Lean cannot check.
            if fuelled_group(id, &ds) {
                return None;
            }
            if induct::divides_down(&def, &ds) {
                if !self.open_halving {
                    return None;
                }
                self.met_halving.set(true);
            }
        }
        Some(def)
    }

    fn read_def(&self, id: FnId) -> Option<Def> {
        let symbols = self.inputs.symbol_table;
        let key = symbols.fn_entry(id).key.clone();
        let scope = key.scope_str();
        let fd = match scope {
            Some(prefix) => self
                .inputs
                .dep_modules
                .iter()
                .find(|m| m.prefix == prefix)?
                .fn_defs
                .iter()
                .find(|f| f.name == key.name)?,
            None => self.inputs.entry_items.iter().find_map(|item| match item {
                crate::ast::TopLevel::FnDef(f) if f.name == key.name => Some(f),
                _ => None,
            })?,
        };
        if !fd.effects.is_empty() {
            return None;
        }
        // `?` read as the `match` the Lean export writes too.
        let expanded = (!self.as_written)
            .then(|| crate::codegen::proof_lower::try_match::expand(&fd.body))
            .flatten();
        let (last, before) = expanded.as_ref().unwrap_or(&fd.body).stmts().split_last()?;
        let Stmt::Expr(body) = last else {
            return None;
        };
        // The parameters, and each local binding from the line after its
        // own, hide a module-level binding of the same name.
        let mut locals: Vec<String> = fd.params.iter().map(|(n, _)| n.clone()).collect();
        let read = |e, locals: &[String]| {
            qualify_bindings(
                &canon(&self.inputs.resolve_expr(e, scope)),
                self.inputs,
                scope,
                locals,
            )
        };
        let mut lets = Vec::new();
        for stmt in before {
            match stmt {
                Stmt::Binding(name, _, value) if name != "_" => {
                    lets.push((name.clone(), read(value, &locals)));
                    locals.push(name.clone());
                }
                _ => return None,
            }
        }
        let body = read(body, &locals);
        let name = match scope {
            Some(prefix) => format!("{prefix}.{}", key.name),
            None => key.name.clone(),
        };
        Some(Def {
            fn_id: id,
            name,
            params: fd.params.iter().map(|(n, _)| n.clone()).collect(),
            param_types: fd
                .params
                .iter()
                .map(|(_, t)| {
                    super::finite::qualify(self.inputs, &crate::types::parse_type_str(t), scope)
                })
                .collect(),
            ret: super::finite::qualify(
                self.inputs,
                &crate::types::parse_type_str(&fd.return_type),
                scope,
            ),
            lets,
            body,
        })
    }

    /// Record that law `law`, at `subst`, matched but the conjuncts `open`
    /// of what it assumes found no proof: an argument the law's author can
    /// add.
    pub(crate) fn note_open_premise(
        &mut self,
        law: &crate::ir::proof_steps::LawRef,
        subst: &[(String, Term)],
        open: &[Term],
    ) {
        let key = &law.key;
        // A law with a given of a refined type also assumes what that type
        // holds of the value put for the given.
        let what = if law.refined.is_empty() {
            "its `when`"
        } else {
            "what it assumes (its `when`, and what the refined types of its givens hold)"
        };
        let names = self.inputs.symbol_table;
        let show = |t: &Term| crate::ir::proof_steps::show::term(t, names);
        let at = subst
            .iter()
            .map(|(g, t)| format!("{g} := {}", show(t)))
            .collect::<Vec<_>>()
            .join(", ");
        let needs = open
            .iter()
            .map(|t| format!("`{}`", show(t)))
            .collect::<Vec<_>>()
            .join(" and ");
        let note = format!(
            "missing argument: law {key} at ({at}) applies but {what} needs {needs}, which no hypothesis, linear step or computation gives; state it in a `because` line or cite a law that does"
        );
        self.note(note);
    }

    /// Record `when`, the one open line of a cited law's `when` where the
    /// law matched, once.
    pub(crate) fn note_open_when(&mut self, when: &Term) {
        if self.trace.is_some() {
            self.traced(super::memo::Effect::OpenWhen(Box::new(when.clone())));
        }
        if !self.open_whens.contains(when) {
            self.open_whens.push(when.clone());
        }
    }

    /// Record that evaluation met a call of `f` it may not open.
    pub(crate) fn note_closed_recursion(&mut self, f: FnId) {
        let name = crate::ir::proof_steps::sexpr::Names::fn_name(self.inputs.symbol_table, f);
        let note = format!(
            "checker limit: steps do not open `{name}`, whose recursion is not a descent they follow"
        );
        self.note(note);
    }

    /// Keep `note` for the refusal, once.
    pub(crate) fn note(&mut self, note: String) {
        if self.trace.is_some() {
            self.traced(super::memo::Effect::Note(note.clone()));
        }
        if !self.open_premises.contains(&note) {
            self.open_premises.push(note);
        }
    }

    /// `why`, with the cited laws whose `when` stayed open and the calls
    /// steps could not open.
    pub(crate) fn with_open_premises(&self, why: String) -> String {
        if self.open_premises.is_empty() {
            why
        } else {
            format!("{why} ({})", self.open_premises.join("; "))
        }
    }

    pub(crate) fn hyp_for(&self, t: &Term) -> Option<(String, Term)> {
        let t = canon_ref(t);
        self.hyps
            .iter()
            .rev()
            .find(|(_, e)| *canon_ref(&e.lhs) == *t)
            .map(|(n, e)| (n.clone(), e.rhs.clone()))
    }
}

/// The name step terms read module `scope`'s binding `name` under:
/// `Module.name` for a dependency, `.name` for the entry module. Either has
/// a dot, which no given, parameter or pattern name has, so a local spelled
/// like a module-level binding is never read as that binding, and unfolding
/// a function that reads the binding never turns it into the local.
pub(crate) fn binding_read(scope: Option<&str>, name: &str) -> String {
    format!("{}.{name}", scope.unwrap_or(""))
}

/// `t`, written in module `scope`, with each read of one of that module's
/// bindings spelled the way every step term spells it ([`binding_read`]).
/// `locals` are the names in scope where `t` is written (a law's givens, a
/// function's parameters and local bindings): each hides a module-level
/// binding of the same name, as a name a pattern binds does in its arm.
pub(crate) fn qualify_bindings(
    t: &Term,
    inputs: &ProofLowerInputs,
    scope: Option<&str>,
    locals: &[String],
) -> Term {
    let mut free = Vec::new();
    crate::ir::proof_steps::term::free_vars(t, &mut free);
    free.retain(|n| !locals.contains(n));
    if free.is_empty() {
        return t.clone();
    }
    let map: Vec<(String, Term)> = crate::codegen::proof_lower::module_bindings_of(inputs, scope)
        .into_iter()
        .filter(|b| free.contains(&b.name))
        .map(|b| {
            let read = crate::ir::proof_steps::term::var(&binding_read(scope, &b.name));
            if let Ok(ty) = b.declared_type() {
                read.set_ty(ty);
            }
            (b.name, read)
        })
        .collect();
    if map.is_empty() {
        return t.clone();
    }
    crate::ir::proof_steps::term::subst(t, &map).unwrap_or_else(|_| t.clone())
}
