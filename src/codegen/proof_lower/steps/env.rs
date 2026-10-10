//! What a producer may consult: source definitions it is allowed to open,
//! cited laws, and the hypotheses in scope.

use std::collections::HashMap;

use crate::ast::Stmt;
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{Term, canon};
use crate::ir::proof_steps::{Const, Def, Eqn, LawRef};

pub(crate) struct Env<'a> {
    pub inputs: &'a ProofLowerInputs<'a>,
    defs: HashMap<FnId, Option<Def>>,
    /// Every pure definition as written, before the termination gate.
    raw_defs: std::cell::RefCell<HashMap<FnId, Option<Def>>>,
    /// Whether a definition counts an Int toward zero, and whether it
    /// divides it on the way, by [`Env::counts_down`].
    countdowns: std::cell::RefCell<HashMap<FnId, (bool, bool)>>,
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
    /// The laws before this one it does not cite, offered as hints where
    /// evaluation stops (never applied).
    pub hint_laws: Vec<LawRef>,
    pub hyps: Vec<(String, Eqn)>,
    pub fuel: usize,
    next_hyp: usize,
    /// Induction hypotheses named so far, so names stay distinct.
    pub next_ih: usize,
    /// Evaluations in progress, one inside another.
    pub nesting: usize,
    /// Builtin facts and earlier laws that would rewrite a part of a term
    /// evaluation stopped at, or the line that names an induction, as
    /// hints for the report (see [`super::eval`]).
    pub hints: Vec<String>,
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
}

impl<'a> Env<'a> {
    pub(crate) fn new(inputs: &'a ProofLowerInputs<'a>) -> Self {
        Self {
            inputs,
            defs: HashMap::new(),
            raw_defs: std::cell::RefCell::new(HashMap::new()),
            countdowns: std::cell::RefCell::new(HashMap::new()),
            used: Vec::new(),
            consts: HashMap::new(),
            used_consts: Vec::new(),
            laws: Vec::new(),
            rewrite_laws: Vec::new(),
            cited_all: Vec::new(),
            fact_rules: Vec::new(),
            hint_laws: Vec::new(),
            hyps: Vec::new(),
            fuel: 4000,
            next_hyp: 0,
            next_ih: 0,
            nesting: 0,
            hints: Vec::new(),
            open_premises: Vec::new(),
            opened: Vec::new(),
            barren: Vec::new(),
            proving_cited_when: false,
            open_halving: true,
            met_halving: std::cell::Cell::new(false),
            givens: Vec::new(),
            finite: Vec::new(),
            split_binders: Vec::new(),
        }
    }

    pub(crate) fn fresh_hyp(&mut self) -> String {
        self.next_hyp += 1;
        format!("h_steps{}", self.next_hyp)
    }

    pub(crate) fn burn(&mut self) -> Result<(), String> {
        if self.fuel == 0 {
            return Err("the producer ran out of fuel".into());
        }
        self.fuel -= 1;
        Ok(())
    }

    /// A definition steps may open: pure, local bindings then one
    /// expression, and, if it calls itself, through the termination gate of
    /// [`crate::ir::proof_steps::induct::recursion`].
    pub(crate) fn def(&mut self, id: FnId) -> Option<Def> {
        if let Some(found) = self.defs.get(&id) {
            return found.clone();
        }
        let built = self.build_def(id);
        self.defs.insert(id, built.clone());
        built
    }

    pub(crate) fn mark_used(&mut self, id: FnId) {
        if !self.used.contains(&id) {
            self.used.push(id);
        }
    }

    pub(crate) fn used_defs(&mut self) -> Vec<Def> {
        let ids = self.used.clone();
        ids.into_iter().filter_map(|id| self.def(id)).collect()
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
        if !self.used_consts.iter().any(|n| n == name) {
            self.used_consts.push(name.to_string());
        }
    }

    pub(crate) fn used_consts(&mut self) -> Vec<Const> {
        let names = self.used_consts.clone();
        names.iter().filter_map(|n| self.constant(n)).collect()
    }

    fn build_const(&self, name: &str) -> Option<Const> {
        let (scope, short) = match name.rsplit_once('.') {
            Some((prefix, short)) => (Some(prefix), short),
            None => (None, name),
        };
        let binding = crate::codegen::proof_lower::module_bindings_of(self.inputs, scope)
            .into_iter()
            .find(|b| b.name == short)?;
        binding.declared_type().ok()?;
        let value = canon(&self.inputs.resolve_expr(&binding.value, scope));
        Some(Const {
            name: name.to_string(),
            value: qualify_bindings(&value, self.inputs, scope),
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
        let (last, before) = fd.body.stmts().split_last()?;
        let Stmt::Expr(body) = last else {
            return None;
        };
        let read = |e| {
            qualify_bindings(
                &canon(&self.inputs.resolve_expr(e, scope)),
                self.inputs,
                scope,
            )
        };
        let mut lets = Vec::new();
        for stmt in before {
            match stmt {
                Stmt::Binding(name, _, value) if name != "_" => {
                    lets.push((name.clone(), read(value)))
                }
                _ => return None,
            }
        }
        let body = read(body);
        let name = match scope {
            Some(prefix) => format!("{prefix}.{}", key.name),
            None => key.name.clone(),
        };
        Some(Def {
            fn_id: id,
            name,
            params: fd.params.iter().map(|(n, _)| n.clone()).collect(),
            returns_bool: fd.return_type == "Bool",
            lets,
            body,
        })
    }

    /// Record that law `key`, at `subst`, matched but the conjuncts `open`
    /// of its `when` found no proof: an argument the law's author can add.
    pub(crate) fn note_open_premise(&mut self, key: &str, subst: &[(String, Term)], open: &[Term]) {
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
            "missing argument: law {key} at ({at}) applies but its `when` needs {needs}, which no hypothesis, linear step or computation gives; state it in a `because` line or cite a law that does"
        );
        if !self.open_premises.contains(&note) {
            self.open_premises.push(note);
        }
    }

    /// Record that evaluation met a call of `f` it may not open.
    pub(crate) fn note_closed_recursion(&mut self, f: FnId) {
        let name = crate::ir::proof_steps::sexpr::Names::fn_name(self.inputs.symbol_table, f);
        let note = format!(
            "checker limit: steps do not open `{name}`, whose recursion is not a descent they follow"
        );
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
        let t = canon(t);
        self.hyps
            .iter()
            .rev()
            .find(|(_, e)| canon(&e.lhs) == t)
            .map(|(n, e)| (n.clone(), e.rhs.clone()))
    }
}

/// `t`, written in module `scope`, with each read of one of that module's
/// bindings spelled the way every step term spells it: bare for the entry
/// module, `Module.name` for a dependency, so a dependency's `base` unfolded
/// into an entry law is not read as the entry's own `base`.
pub(crate) fn qualify_bindings(t: &Term, inputs: &ProofLowerInputs, scope: Option<&str>) -> Term {
    let Some(prefix) = scope else {
        return t.clone();
    };
    let map: Vec<(String, Term)> =
        crate::codegen::proof_lower::module_bindings_of(inputs, Some(prefix))
            .into_iter()
            .map(|b| {
                let read = crate::ir::proof_steps::term::var(&format!("{prefix}.{}", b.name));
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
