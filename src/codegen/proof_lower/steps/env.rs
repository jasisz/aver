//! What a producer may consult: source definitions it is allowed to open,
//! cited laws, and the hypotheses in scope.

use std::collections::HashMap;

use crate::ast::Stmt;
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{Term, canon};
use crate::ir::proof_steps::{Def, Eqn, LawRef};

pub(crate) struct Env<'a> {
    pub inputs: &'a ProofLowerInputs<'a>,
    defs: HashMap<FnId, Option<Def>>,
    /// Definitions opened so far, in first-use order.
    pub used: Vec<FnId>,
    pub laws: Vec<LawRef>,
    pub hyps: Vec<(String, Eqn)>,
    pub fuel: usize,
    next_hyp: usize,
}

impl<'a> Env<'a> {
    pub(crate) fn new(inputs: &'a ProofLowerInputs<'a>) -> Self {
        Self {
            inputs,
            defs: HashMap::new(),
            used: Vec::new(),
            laws: Vec::new(),
            hyps: Vec::new(),
            fuel: 4000,
            next_hyp: 0,
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

    /// A definition steps may open: pure, non-recursive, one expression.
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

    fn build_def(&self, id: FnId) -> Option<Def> {
        let symbols = self.inputs.symbol_table;
        if self.inputs.recursive_fns.contains(&id) {
            return None;
        }
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
        let [Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let body = canon(&self.inputs.resolve_expr(body, scope));
        let name = match scope {
            Some(prefix) => format!("{prefix}.{}", key.name),
            None => key.name.clone(),
        };
        Some(Def {
            fn_id: id,
            name,
            params: fd.params.iter().map(|(n, _)| n.clone()).collect(),
            body,
        })
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
