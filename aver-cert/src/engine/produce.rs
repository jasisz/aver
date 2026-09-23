// ---- the schema-9 producer ---------------------------------------------------
//
// Takes the compiler's printed plans (`ModulePlans`) and the exact module bytes,
// and decides which functions the certificate OFFERS: a function is offered
// when its plan types, lowers to exactly its code entry, has exactly its
// declared function type, cites only confirmed layout, and calls only offered
// functions. Everything else is declined per function with a reason. The
// offered set is grouped into call SCCs (callees first) and rendered; the
// wall re-checks all of it.

/// One planned function of the package, in `fnPlans` order.
pub struct PackageEntry {
    /// Export name, or `#<funcIdx>` for an internal callee.
    pub name: String,
    pub exported: bool,
    pub func_idx: u32,
    pub group: u32,
    pub plan: FnPlan,
}

/// The report data of one certified export, as the wall derives it
/// (`AcceptedArtifact.obligationsOf`, `ClaimAxes.reportFacets`). The checker
/// witness pins every field.
pub struct CertifiedExport {
    pub name: String,
    pub func_idx: u32,
    pub total: bool,
    pub facets: Vec<&'static str>,
}

pub struct Analysis {
    carrier: Option<u32>,
    roles: Option<HostRoles>,
    string_roles: StringHostRoles,
    entries: Vec<PackageEntry>,
    types: PlanTypeTable,
    certified: Vec<CertifiedExport>,
    declined: Vec<(String, String)>,
    contracts: Vec<String>,
    module_envelope: ModuleEnvelopeFacts,
}

impl Analysis {
    pub fn certified_names(&self) -> Vec<String> {
        self.certified.iter().map(|c| c.name.clone()).collect()
    }

    pub fn declined(&self) -> &[(String, String)] {
        &self.declined
    }

    pub fn entries(&self) -> &[PackageEntry] {
        &self.entries
    }

    pub fn certified(&self) -> &[CertifiedExport] {
        &self.certified
    }
}

/// A decline reason the checker's display gate admits: printable ASCII
/// without quotes or backslashes, within the candidate length.
fn clean_reason(reason: &str) -> String {
    let mut out: String = reason
        .chars()
        .map(|c| {
            if c.is_ascii() && (' '..='~').contains(&c) && c != '"' && c != '\\' {
                c
            } else {
                '?'
            }
        })
        .collect();
    let max = crate::format::MAX_CANDIDATE_LEN;
    if out.len() > max {
        out.truncate(max - 3);
        out.push_str("...");
    }
    out
}

fn is_runtime_export(name: &str) -> bool {
    name.starts_with("__")
        || name == "_start"
        || name == "memory"
        || name.contains('#')
        || name.contains(':')
}

// ---- type table confirmation (`TypeTable.typeTableConfirmed`) ----

impl ModuleFacts {
    fn group_entry(&self, idx: u64) -> Option<&TypeFact> {
        (idx < self.first_group_len as u64).then(|| &self.types[idx as usize])
    }

    fn struct_is(&self, m: &MCtx<'_>, idx: u64, ts: &[PlanTy]) -> bool {
        let Some(TypeFact {
            comp: CompT::Struct(fields),
            ..
        }) = self.group_entry(idx)
        else {
            return false;
        };
        let want: Option<Vec<StorT>> = ts.iter().map(|t| m.val_t(t).map(StorT::Val)).collect();
        want.as_deref() == Some(fields.as_slice())
    }

    fn array_is(&self, idx: u64, st: StorT) -> bool {
        matches!(self.group_entry(idx), Some(TypeFact { comp: CompT::Array(s), .. }) if *s == st)
    }
}

/// Drop every type-table entry the type section does not confirm, until the
/// table is stable (a dropped entry makes the entries that name it fail too).
fn confirm_type_table(facts: &ModuleFacts, tt: &mut PlanTypeTable) {
    if facts.first_group_len == 0 {
        *tt = PlanTypeTable::default();
        return;
    }
    // The carrier declaration is the byte carrier and its limb array.
    let carrier_ok = match (facts.carrier, tt.carrier, tt.mag) {
        (Some(c), Some(c2), Some(m)) => {
            c == c2
                && facts.array_is(u64::from(m), StorT::Val(ValT::I64))
                && matches!(facts.group_entry(u64::from(c)),
                    Some(TypeFact { comp: CompT::Struct(fs), .. }) if fs.get(1) == Some(&StorT::Val(ValT::RefNull(m))))
        }
        (None, None, None) => true,
        _ => false,
    };
    if !carrier_ok {
        tt.carrier = None;
        tt.mag = None;
    }
    if let Some(s) = tt.str_
        && !facts.array_is(u64::from(s), StorT::I8)
    {
        tt.str_ = None;
    }
    match (tt.str_vec, tt.str_) {
        (Some(v), Some(s)) if facts.array_is(u64::from(v), StorT::Val(ValT::RefNull(s))) => {}
        _ => tt.str_vec = None,
    }
    loop {
        let snapshot = tt.clone();
        let m = MCtx::new(None, &Vec::new(), &snapshot, &[]);
        let before = (
            tt.records.len(),
            tt.sums.len(),
            tt.options.len(),
            tt.results.len(),
            tt.vecs.len(),
            tt.lists.len(),
            tt.opaques.len(),
        );
        tt.records.retain(|r| match r.fields.as_slice() {
            [f] => m.val_t(f) == Some(ValT::RefNull(r.struct_idx)),
            fs => fs.len() >= 2 && facts.struct_is(&m, u64::from(r.struct_idx), fs),
        });
        tt.sums.retain(|d| {
            let root_ok = matches!(facts.group_entry(u64::from(d.root)),
                Some(TypeFact { comp: CompT::Struct(fs), is_final: false, supertype: None }) if fs.is_empty());
            root_ok
                && m.sum_ok(d.tid)
                && d.ctors.iter().all(|(idx, fs)| {
                    facts.struct_is(&m, u64::from(*idx), fs)
                        && matches!(facts.group_entry(u64::from(*idx)),
                            Some(TypeFact { is_final: true, supertype: Some(root), .. }) if *root == d.root)
                })
        });
        tt.options
            .retain(|(t, idx)| facts.struct_is(&m, u64::from(*idx), &[PlanTy::Bool, t.clone()]));
        tt.results.retain(|(t, e, idx)| {
            facts.struct_is(&m, u64::from(*idx), &[PlanTy::Bool, t.clone(), e.clone()])
        });
        tt.lists.retain(|(t, idx)| {
            facts.struct_is(
                &m,
                u64::from(*idx),
                &[t.clone(), PlanTy::List(Box::new(t.clone()))],
            )
        });
        tt.vecs.retain(|(t, idx)| match m.val_t(t) {
            Some(v) => facts.array_is(u64::from(*idx), StorT::Val(v)),
            None => false,
        });
        tt.opaques
            .retain(|(_, idx)| (*idx as usize) < facts.first_group_len);
        // No struct index serves two declarations: keep the first.
        let mut owned = BTreeSet::new();
        if let Some(c) = tt.carrier {
            owned.insert(c);
        }
        for x in [tt.mag, tt.str_, tt.str_vec].into_iter().flatten() {
            owned.insert(x);
        }
        tt.records
            .retain(|r| r.fields.len() < 2 || owned.insert(r.struct_idx));
        tt.sums.retain(|d| {
            let mine: Vec<u32> = std::iter::once(d.root)
                .chain(d.ctors.iter().map(|c| c.0))
                .collect();
            if mine.iter().all(|x| !owned.contains(x))
                && mine.iter().collect::<BTreeSet<_>>().len() == mine.len()
            {
                owned.extend(mine);
                true
            } else {
                false
            }
        });
        tt.options.retain(|o| owned.insert(o.1));
        tt.results.retain(|r| owned.insert(r.2));
        tt.lists.retain(|l| owned.insert(l.1));
        tt.vecs.retain(|v| owned.insert(v.1));
        tt.str_segs
            .retain(|(b, seg)| facts.data.get(*seg as usize) == Some(&Some(b.clone())));
        let after = (
            tt.records.len(),
            tt.sums.len(),
            tt.options.len(),
            tt.results.len(),
            tt.vecs.len(),
            tt.lists.len(),
            tt.opaques.len(),
        );
        if before == after {
            return;
        }
    }
}

// ---- which types a plan cites ----

#[derive(Default)]
struct Cited {
    tys: BTreeSet<PlanTy>,
    segs: BTreeSet<Vec<u8>>,
}

impl Cited {
    fn ty(&mut self, t: &PlanTy) {
        if !self.tys.insert(t.clone()) {
            return;
        }
        match t {
            PlanTy::Option(x) | PlanTy::Vec(x) | PlanTy::List(x) => self.ty(x),
            PlanTy::Result(x, e) => {
                self.ty(x);
                self.ty(e);
            }
            _ => {}
        }
    }

    fn expr(&mut self, e: &PlanExpr) {
        match e {
            PlanExpr::Literal(PlanLit::Str(b)) => {
                self.segs.insert(b.clone());
                self.ty(&PlanTy::Str);
            }
            PlanExpr::Literal(PlanLit::Int(_)) => self.ty(&PlanTy::Int),
            PlanExpr::Literal(_) | PlanExpr::Local(_) => {}
            PlanExpr::Let(_, v, b) => {
                self.expr(v);
                self.expr(b);
            }
            PlanExpr::Call(_, args) | PlanExpr::TailCall(_, args) | PlanExpr::Interp(args) => {
                args.iter().for_each(|a| self.expr(a))
            }
            PlanExpr::BinOp(_, l, r) => {
                self.expr(l);
                self.expr(r);
            }
            PlanExpr::Neg(x) => self.expr(x),
            PlanExpr::If(c, t, el) => {
                self.expr(c);
                self.expr(t);
                self.expr(el);
            }
            PlanExpr::RecordCreate(tid, fs) => {
                self.ty(&PlanTy::Record(*tid));
                fs.iter().for_each(|a| self.expr(a));
            }
            PlanExpr::Project(tid, _, b) => {
                self.ty(&PlanTy::Record(*tid));
                self.expr(b);
            }
            PlanExpr::Match(s, arms) => {
                self.expr(s);
                for (p, b) in arms {
                    match p {
                        PlanPat::LitStr(k) => {
                            self.segs.insert(k.clone());
                        }
                        PlanPat::Ctor(PlanCtor::User(tid, _), _) => self.ty(&PlanTy::Sum(*tid)),
                        _ => {}
                    }
                    self.expr(b);
                }
            }
            PlanExpr::Construct(c, ty, args) => {
                if let PlanCtor::User(tid, _) = c {
                    self.ty(&PlanTy::Sum(*tid));
                }
                self.ty(ty);
                args.iter().for_each(|a| self.expr(a));
            }
            PlanExpr::List(t, items) => {
                self.ty(&PlanTy::List(Box::new(t.clone())));
                items.iter().for_each(|a| self.expr(a));
            }
        }
    }

    fn plan(&mut self, p: &FnPlan) {
        p.params.iter().for_each(|t| self.ty(t));
        self.ty(&p.ret);
        p.locals.iter().for_each(|t| self.ty(t));
        self.expr(&p.body);
    }

    /// Close over the declarations: a record's and a constructor's field
    /// types are cited too.
    fn close(&mut self, tt: &PlanTypeTable) {
        loop {
            let before = self.tys.len();
            let snapshot: Vec<PlanTy> = self.tys.iter().cloned().collect();
            for t in snapshot {
                match t {
                    PlanTy::Record(tid) => {
                        if let Some(r) = tt.records.iter().find(|r| r.tid == tid) {
                            r.fields.iter().for_each(|f| self.ty(f));
                        }
                    }
                    PlanTy::Sum(tid) => {
                        if let Some(s) = tt.sums.iter().find(|s| s.tid == tid) {
                            s.ctors
                                .iter()
                                .flat_map(|c| c.1.iter())
                                .for_each(|f| self.ty(f));
                        }
                    }
                    _ => {}
                }
            }
            if self.tys.len() == before {
                return;
            }
        }
    }

    /// The table restricted to what is cited.
    fn restrict(&self, tt: &PlanTypeTable) -> PlanTypeTable {
        let has = |t: PlanTy| self.tys.contains(&t);
        PlanTypeTable {
            carrier: tt.carrier,
            mag: tt.mag,
            str_: tt.str_,
            str_vec: tt.str_vec,
            records: tt
                .records
                .iter()
                .filter(|r| has(PlanTy::Record(r.tid)))
                .cloned()
                .collect(),
            sums: tt
                .sums
                .iter()
                .filter(|s| has(PlanTy::Sum(s.tid)))
                .cloned()
                .collect(),
            options: tt
                .options
                .iter()
                .filter(|o| has(PlanTy::Option(Box::new(o.0.clone()))))
                .cloned()
                .collect(),
            results: tt
                .results
                .iter()
                .filter(|r| has(PlanTy::Result(Box::new(r.0.clone()), Box::new(r.1.clone()))))
                .cloned()
                .collect(),
            vecs: tt
                .vecs
                .iter()
                .filter(|v| has(PlanTy::Vec(Box::new(v.0.clone()))))
                .cloned()
                .collect(),
            lists: tt
                .lists
                .iter()
                .filter(|l| has(PlanTy::List(Box::new(l.0.clone()))))
                .cloned()
                .collect(),
            opaques: tt
                .opaques
                .iter()
                .filter(|o| has(PlanTy::Opaque(o.0)))
                .cloned()
                .collect(),
            str_segs: tt
                .str_segs
                .iter()
                .filter(|s| self.segs.contains(&s.0))
                .cloned()
                .collect(),
        }
    }
}

/// Tarjan's SCCs over `nodes` (call edges restricted to `nodes`), emitted
/// callees first.
fn call_sccs(nodes: &[u32], edges: &BTreeMap<u32, Vec<u32>>) -> Vec<Vec<u32>> {
    struct St<'a> {
        edges: &'a BTreeMap<u32, Vec<u32>>,
        index: BTreeMap<u32, usize>,
        low: BTreeMap<u32, usize>,
        stack: Vec<u32>,
        on: BTreeSet<u32>,
        next: usize,
        out: Vec<Vec<u32>>,
    }
    fn visit(st: &mut St<'_>, v: u32) {
        st.index.insert(v, st.next);
        st.low.insert(v, st.next);
        st.next += 1;
        st.stack.push(v);
        st.on.insert(v);
        for w in st.edges.get(&v).cloned().unwrap_or_default() {
            if !st.index.contains_key(&w) {
                visit(st, w);
                let lw = st.low[&w];
                let lv = st.low[&v];
                st.low.insert(v, lv.min(lw));
            } else if st.on.contains(&w) {
                let iw = st.index[&w];
                let lv = st.low[&v];
                st.low.insert(v, lv.min(iw));
            }
        }
        if st.low[&v] == st.index[&v] {
            let mut comp = Vec::new();
            loop {
                let w = st.stack.pop().expect("tarjan stack");
                st.on.remove(&w);
                comp.push(w);
                if w == v {
                    break;
                }
            }
            comp.sort_unstable();
            st.out.push(comp);
        }
    }
    let mut st = St {
        edges,
        index: BTreeMap::new(),
        low: BTreeMap::new(),
        stack: Vec::new(),
        on: BTreeSet::new(),
        next: 0,
        out: Vec::new(),
    };
    for &v in nodes {
        if !st.index.contains_key(&v) {
            visit(&mut st, v);
        }
    }
    st.out
}

/// Check one candidate against the bytes: typing, the code entry, the
/// function type, the string literals, and that every call targets a
/// candidate. `None` means offered.
fn check_candidate(
    facts: &ModuleFacts,
    m: &MCtx<'_>,
    func_idx: u32,
    plan: &FnPlan,
    candidates: &BTreeSet<u32>,
) -> Option<String> {
    let mut targets = Vec::new();
    call_targets(&plan.body, &mut targets);
    if let Some(t) = targets.iter().find(|t| !candidates.contains(t)) {
        return Some(format!("calls function {t}, which has no certified plan"));
    }
    if !plan_typed(m, plan) {
        return Some("plan does not type in the one grammar".into());
    }
    let Some(bytes) = m.code_entry_bytes(plan) else {
        return Some("plan lowering cites an undeclared index or type".into());
    };
    let Some(code) = facts.code_of(func_idx) else {
        return Some("no code entry at the function index".into());
    };
    if code.entry != bytes {
        let at = code
            .entry
            .iter()
            .zip(&bytes)
            .take_while(|(a, b)| a == b)
            .count();
        return Some(format!(
            "plan lowering differs from the emitted code entry at byte {at} ({} vs {} bytes)",
            bytes.len(),
            code.entry.len()
        ));
    }
    let sig_ok = match (
        plan.params
            .iter()
            .map(|t| m.val_t(t))
            .collect::<Option<Vec<_>>>(),
        m.val_t(&plan.ret),
        facts.fn_sig(func_idx),
    ) {
        (Some(ps), Some(r), Some(CompT::Func(fp, fr))) => &ps == fp && fr.as_slice() == [r],
        _ => false,
    };
    if !sig_ok {
        return Some("declared function type is not the plan's signature".into());
    }
    let mut lits = Vec::new();
    string_lits(&plan.body, &mut lits);
    for b in lits {
        let seg = m.str_seg(b);
        if seg >= 4_294_967_296 || facts.data.get(seg as usize) != Some(&Some(b.to_vec())) {
            return Some("a string literal's data segment is not confirmed".into());
        }
    }
    None
}

/// Analyze one core module against the compiler's plans.
pub fn analyze(
    core_bytes: &[u8],
    plans: &ModulePlans,
    artifact_target: &str,
) -> Result<Analysis, String> {
    let facts = ModuleFacts::parse(core_bytes)?;
    let carrier = facts.carrier;
    let mut roles = facts.roles;
    let carriered = roles.box_idx.is_some();
    if carriered && roles.arith_params(carrier).is_none() {
        return Err("module exports the Int box helper but its arithmetic helper indices do not resolve; the host-role table cannot be declared".into());
    }
    // `__aint_eq` is pinned by template, so a module that does not export it
    // may still declare it: the index is the compiler's (producer data).
    let eq_from_hint = carriered && roles.eq_idx.is_none() && plans.aint_eq_idx.is_some();
    if eq_from_hint {
        roles.eq_idx = plans.aint_eq_idx;
    }
    let role_table = carriered.then_some(roles);

    let mut export_name: BTreeMap<u32, String> = BTreeMap::new();
    for (name, kind, idx) in &facts.exports {
        if *kind == 0 && !is_runtime_export(name) && name.is_ascii() {
            export_name.entry(*idx).or_insert_with(|| name.clone());
        }
    }

    let mut types = plans.types.clone();
    confirm_type_table(&facts, &mut types);

    let mut reasons: BTreeMap<u32, String> = BTreeMap::new();
    let mut plan_of: BTreeMap<u32, &FnPlan> = BTreeMap::new();
    for f in &plans.fns {
        match &f.plan {
            Ok(p) => {
                plan_of.insert(f.func_idx, p);
            }
            Err(reason) => {
                reasons.insert(f.func_idx, reason.clone());
            }
        }
    }
    let mut candidates: BTreeSet<u32> = plan_of.keys().copied().collect();
    loop {
        let fns: Vec<(u32, &FnPlan)> = candidates.iter().map(|f| (*f, plan_of[f])).collect();
        let m = MCtx::new(role_table.as_ref(), &facts.string_roles, &types, &fns);
        let mut dropped = Vec::new();
        for (f, p) in &fns {
            if let Some(reason) = check_candidate(&facts, &m, *f, p, &candidates) {
                dropped.push((*f, reason));
            }
        }
        if dropped.is_empty() {
            break;
        }
        for (f, reason) in dropped {
            candidates.remove(&f);
            reasons.insert(f, reason);
        }
    }

    // Offered: every exported candidate, plus the internal candidates it
    // reaches.
    let mut edges: BTreeMap<u32, Vec<u32>> = BTreeMap::new();
    for f in &candidates {
        let mut t = Vec::new();
        call_targets(&plan_of[f].body, &mut t);
        t.sort_unstable();
        t.dedup();
        edges.insert(*f, t);
    }
    let mut included: BTreeSet<u32> = BTreeSet::new();
    let mut work: Vec<u32> = candidates
        .iter()
        .copied()
        .filter(|f| export_name.contains_key(f))
        .collect();
    while let Some(f) = work.pop() {
        if included.insert(f) {
            work.extend(edges[&f].iter().copied());
        }
    }
    let nodes: Vec<u32> = included.iter().copied().collect();
    let sccs = call_sccs(&nodes, &edges);
    let mut entries = Vec::new();
    for (group, comp) in sccs.iter().enumerate() {
        for f in comp {
            let exported = export_name.get(f);
            entries.push(PackageEntry {
                name: exported.cloned().unwrap_or_else(|| format!("#{f}")),
                exported: exported.is_some(),
                func_idx: *f,
                group: group as u32,
                plan: plan_of[f].clone(),
            });
        }
    }

    // Lowered helper calls (for the contracts and the eq role).
    let fns: Vec<(u32, &FnPlan)> = entries.iter().map(|e| (e.func_idx, &e.plan)).collect();
    let mut role_table = role_table;
    let m = MCtx::new(role_table.as_ref(), &facts.string_roles, &types, &fns);
    let mut calls = Vec::new();
    for e in &entries {
        lowered_calls(&m.lower_plan(&e.plan), &mut calls);
    }
    if eq_from_hint
        && !calls.contains(&m.eq)
        && let Some(r) = role_table.as_mut()
    {
        r.eq_idx = None;
    }

    // Policies per call group.
    let mut group_total: BTreeMap<u32, Option<bool>> = BTreeMap::new();
    for (group, comp) in sccs.iter().enumerate() {
        let members: Vec<(u32, &FnPlan)> = comp.iter().map(|f| (*f, plan_of[f])).collect();
        group_total.insert(group as u32, check_term_group(&members));
    }
    let mut certified = Vec::new();
    for e in entries.iter().filter(|e| e.exported) {
        let members: Vec<&PackageEntry> = entries.iter().filter(|x| x.group == e.group).collect();
        let member_idx: BTreeSet<u32> = members.iter().map(|x| x.func_idx).collect();
        let recursive = members.iter().any(|x| {
            let mut t = Vec::new();
            call_targets(&x.plan.body, &mut t);
            t.iter().any(|t| member_idx.contains(t))
        });
        let mut body = BTreeSet::new();
        facets_e(&e.plan.body, &mut body);
        let facets = [
            "recursive",
            "mutual",
            "calls",
            "records",
            "variants",
            "strings",
            "floats",
        ]
        .into_iter()
        .filter(|f| match *f {
            "recursive" => recursive,
            "mutual" => recursive && members.len() >= 2,
            other => body.contains(other),
        })
        .collect();
        certified.push(CertifiedExport {
            name: e.name.clone(),
            func_idx: e.func_idx,
            total: group_total[&e.group].is_some(),
            facets,
        });
    }

    // Contracts (`ClaimAxes.contractUse`).
    let any_total = certified.iter().any(|c| c.total);
    let any_total_mul = entries
        .iter()
        .filter(|e| e.exported)
        .any(|e| group_total[&e.group] == Some(true));
    let has = |i: u64| calls.contains(&i);
    let mut contracts = Vec::new();
    for (used, name) in [
        (has(m.box_), BOX_CONTRACT),
        (has(m.add), INT_ADD_CONTRACT),
        (has(m.sub), INT_SUB_CONTRACT),
        (has(m.mul), INT_MUL_CONTRACT),
        (has(m.streq), STRING_EQ_CONTRACT),
        (has(m.concat), STRING_CONCAT_CONTRACT),
        (has(m.to_index), TO_INDEX_CONTRACT),
        (has(m.cmp), CMP_CONTRACT),
        (has(m.eq), EQ_CONTRACT),
        (any_total, INT_ADD_TOTAL_CONTRACT),
        (any_total, INT_SUB_TOTAL_CONTRACT),
        (any_total_mul, INT_MUL_TOTAL_CONTRACT),
    ] {
        if used {
            contracts.push(name.to_string());
        }
    }

    let mut cited = Cited::default();
    entries.iter().for_each(|e| cited.plan(&e.plan));
    cited.close(&types);
    let types = cited.restrict(&types);

    let declined: Vec<(String, String)> = export_name
        .iter()
        .filter(|(f, _)| !included.contains(f))
        .map(|(f, name)| {
            let reason = reasons
                .get(f)
                .cloned()
                .unwrap_or_else(|| "no plan was printed for this function".to_string());
            (name.clone(), clean_reason(&reason))
        })
        .collect();

    let certified_pairs: Vec<(String, u32)> = certified
        .iter()
        .map(|c| (c.name.clone(), c.func_idx))
        .collect();
    let module_envelope =
        collect_module_envelope_facts(core_bytes, &certified_pairs, artifact_target)?;

    Ok(Analysis {
        carrier,
        roles: role_table,
        string_roles: facts.string_roles.clone(),
        entries,
        types,
        certified,
        declined,
        contracts,
        module_envelope,
    })
}

#[cfg(test)]
mod produce_tests {
    use super::*;

    fn fixture(name: &str) -> Vec<u8> {
        let path = format!(
            "{}/../prompts/probe-artifacts/one-grammar-p4/fixture/{name}.wasm",
            env!("CARGO_MANIFEST_DIR")
        );
        std::fs::read(&path).unwrap_or_else(|e| panic!("read {path}: {e}"))
    }

    fn l(i: u32) -> PlanExpr {
        PlanExpr::Local(i)
    }

    fn k(v: i64) -> PlanExpr {
        PlanExpr::Literal(PlanLit::Int(v))
    }

    fn bin(op: PlanBinOp, a: PlanExpr, b: PlanExpr) -> PlanExpr {
        PlanExpr::BinOp(op, Box::new(a), Box::new(b))
    }

    fn rec(params: usize, base: PlanExpr, step: PlanExpr) -> FnPlan {
        FnPlan {
            params: vec![PlanTy::Int; params],
            ret: PlanTy::Int,
            nslots: params as u32,
            locals: vec![PlanTy::Int],
            body: PlanExpr::If(
                Box::new(bin(PlanBinOp::Lte, l(0), k(0))),
                Box::new(base),
                Box::new(step),
            ),
        }
    }

    fn desc() -> PlanExpr {
        bin(PlanBinOp::Sub, l(0), k(1))
    }

    fn int_table() -> PlanTypeTable {
        PlanTypeTable {
            carrier: Some(2),
            mag: Some(1),
            str_: Some(0),
            ..PlanTypeTable::default()
        }
    }

    /// The two certprobe2 plans, exactly as the compiler prints them.
    fn certprobe2_plans(sum_to: FnPlan) -> ModulePlans {
        let count_down = rec(
            2,
            l(1),
            PlanExpr::TailCall(2, vec![desc(), bin(PlanBinOp::Add, l(1), l(0))]),
        );
        ModulePlans {
            fns: vec![
                PlannedFn {
                    name: "sumTo".into(),
                    func_idx: 1,
                    plan: Ok(sum_to),
                },
                PlannedFn {
                    name: "countDown".into(),
                    func_idx: 2,
                    plan: Ok(count_down),
                },
            ],
            types: int_table(),
            aint_eq_idx: None,
        }
    }

    fn sum_to() -> FnPlan {
        rec(
            1,
            k(0),
            bin(
                PlanBinOp::Add,
                l(0),
                PlanExpr::Call(PlanCallee::Fn(1), vec![desc()]),
            ),
        )
    }

    #[test]
    fn twin_lowering_reproduces_the_certprobe2_code_entries() {
        let analysis = analyze(
            &fixture("certprobe2"),
            &certprobe2_plans(sum_to()),
            crate::format::TARGET_WASM_GC,
        )
        .expect("certprobe2 analyzes");
        assert_eq!(analysis.certified_names(), ["sumTo", "countDown"]);
        assert!(analysis.certified().iter().all(|c| c.total));
        assert_eq!(analysis.certified()[0].facets, ["recursive", "calls"]);
        assert_eq!(
            analysis.contracts,
            [
                BOX_CONTRACT,
                INT_ADD_CONTRACT,
                INT_SUB_CONTRACT,
                INT_ADD_TOTAL_CONTRACT,
                INT_SUB_TOTAL_CONTRACT
            ]
        );
        let roles = analysis.roles.expect("carriered");
        assert_eq!(
            (roles.box_idx, roles.add_idx, roles.sub_idx),
            (Some(7), Some(8), Some(9))
        );
        assert_eq!(analysis.module_envelope.closure.roots, [1, 2]);
        assert!(analysis.declined().is_empty());
    }

    #[test]
    fn a_plan_that_is_not_the_code_entry_declines_with_the_byte_offset() {
        // A descent by two: the plan types, but its lowering is not the bytes.
        let bad = rec(
            1,
            k(0),
            bin(
                PlanBinOp::Add,
                l(0),
                PlanExpr::Call(PlanCallee::Fn(1), vec![bin(PlanBinOp::Sub, l(0), k(2))]),
            ),
        );
        let analysis = analyze(
            &fixture("certprobe2"),
            &certprobe2_plans(bad),
            crate::format::TARGET_WASM_GC,
        )
        .expect("analyzes");
        assert_eq!(analysis.certified_names(), ["countDown"]);
        let (name, reason) = &analysis.declined()[0];
        assert_eq!(name, "sumTo");
        assert!(
            reason.contains("differs from the emitted code entry at byte"),
            "{reason}"
        );
    }

    #[test]
    fn a_call_to_a_declined_function_declines_the_caller() {
        let mut plans = certprobe2_plans(sum_to());
        plans.fns[1].plan = Err("Neg (no Int negation helper template)".into());
        // `sumTo` still stands alone; make `countDown`'s plan call `sumTo`
        // and decline `sumTo` instead.
        let mut plans2 = certprobe2_plans(sum_to());
        plans2.fns[0].plan = Err("printer declined".into());
        plans2.fns[1].plan = Ok(rec(
            1,
            k(0),
            PlanExpr::Call(PlanCallee::Fn(1), vec![desc()]),
        ));
        let analysis = analyze(
            &fixture("certprobe2"),
            &plans2,
            crate::format::TARGET_WASM_GC,
        )
        .expect("analyzes");
        assert!(analysis.certified_names().is_empty());
        let reasons: BTreeMap<String, String> = analysis.declined().iter().cloned().collect();
        assert_eq!(reasons["sumTo"], "printer declined");
        assert_eq!(
            reasons["countDown"],
            "calls function 1, which has no certified plan"
        );
        let analysis = analyze(
            &fixture("certprobe2"),
            &plans,
            crate::format::TARGET_WASM_GC,
        )
        .expect("analyzes");
        assert_eq!(analysis.certified_names(), ["sumTo"]);
    }

    #[test]
    fn variant_plans_and_their_sum_layout_are_confirmed() {
        let shape = PlanTy::Sum(0);
        let arm = |c: u32, bs: Vec<u32>| PlanPat::Ctor(PlanCtor::User(0, c), bs);
        let mk_circle = FnPlan {
            params: vec![PlanTy::Int],
            ret: shape.clone(),
            nslots: 1,
            locals: vec![PlanTy::Int],
            body: PlanExpr::Construct(PlanCtor::User(0, 0), shape.clone(), vec![l(0)]),
        };
        let area = FnPlan {
            params: vec![shape.clone()],
            ret: PlanTy::Int,
            nslots: 4,
            locals: vec![
                PlanTy::Int,
                PlanTy::Int,
                PlanTy::Int,
                PlanTy::Eqref,
                PlanTy::Int,
            ],
            body: PlanExpr::Match(
                Box::new(l(0)),
                vec![
                    (arm(0, vec![1]), bin(PlanBinOp::Mul, l(1), l(1))),
                    (arm(1, vec![2, 3]), bin(PlanBinOp::Mul, l(2), l(3))),
                    (arm(2, vec![]), k(0)),
                ],
            ),
        };
        let is_dot = FnPlan {
            params: vec![shape.clone()],
            ret: PlanTy::Bool,
            nslots: 1,
            locals: vec![PlanTy::Eqref, PlanTy::Int],
            body: PlanExpr::Match(
                Box::new(l(0)),
                vec![
                    (arm(2, vec![]), PlanExpr::Literal(PlanLit::Bool(true))),
                    (PlanPat::Wild, PlanExpr::Literal(PlanLit::Bool(false))),
                ],
            ),
        };
        let types = PlanTypeTable {
            carrier: Some(6),
            mag: Some(5),
            str_: Some(4),
            sums: vec![PlanSumDecl {
                tid: 0,
                root: 0,
                ctors: vec![
                    (1, vec![PlanTy::Int]),
                    (2, vec![PlanTy::Int, PlanTy::Int]),
                    (3, vec![]),
                ],
            }],
            ..PlanTypeTable::default()
        };
        let plans = ModulePlans {
            fns: vec![
                PlannedFn {
                    name: "mkCircle".into(),
                    func_idx: 1,
                    plan: Ok(mk_circle),
                },
                PlannedFn {
                    name: "area".into(),
                    func_idx: 4,
                    plan: Ok(area),
                },
                PlannedFn {
                    name: "isDot".into(),
                    func_idx: 5,
                    plan: Ok(is_dot),
                },
            ],
            types,
            aint_eq_idx: None,
        };
        let analysis =
            analyze(&fixture("variants"), &plans, crate::format::TARGET_WASM_GC).expect("analyzes");
        assert_eq!(analysis.certified_names(), ["mkCircle", "area", "isDot"]);
        assert!(
            analysis
                .certified()
                .iter()
                .all(|c| c.facets == ["variants"])
        );
        assert_eq!(analysis.types.sums.len(), 1);

        // A constructor declared on another constructor's struct is not
        // confirmed, so every plan citing the sum declines.
        let mut shared = plans.clone();
        shared.types.sums[0].ctors[2].0 = 1;
        let analysis = analyze(&fixture("variants"), &shared, crate::format::TARGET_WASM_GC)
            .expect("analyzes");
        assert!(analysis.certified_names().is_empty());
        assert!(analysis.types.sums.is_empty());
    }

    #[test]
    fn string_interpolation_plans_cite_their_data_segments() {
        let s = |b: &[u8]| PlanExpr::Literal(PlanLit::Str(b.to_vec()));
        let greet = FnPlan {
            params: vec![PlanTy::Str],
            ret: PlanTy::Str,
            nslots: 1,
            locals: vec![],
            body: PlanExpr::Interp(vec![s(b"Hello, "), l(0)]),
        };
        let shout = FnPlan {
            params: vec![PlanTy::Str],
            ret: PlanTy::Str,
            nslots: 1,
            locals: vec![],
            body: PlanExpr::Interp(vec![l(0), s(b"!!!")]),
        };
        let types = PlanTypeTable {
            str_: Some(0),
            str_vec: Some(1),
            str_segs: vec![(b"Hello, ".to_vec(), 0), (b"!!!".to_vec(), 1)],
            ..PlanTypeTable::default()
        };
        let plans = ModulePlans {
            fns: vec![
                PlannedFn {
                    name: "greet".into(),
                    func_idx: 2,
                    plan: Ok(greet),
                },
                PlannedFn {
                    name: "shout".into(),
                    func_idx: 3,
                    plan: Ok(shout),
                },
            ],
            types,
            aint_eq_idx: None,
        };
        let analysis =
            analyze(&fixture("hello"), &plans, crate::format::TARGET_WASM_GC).expect("analyzes");
        assert_eq!(analysis.certified_names(), ["greet", "shout"]);
        assert!(analysis.roles.is_none(), "carrier-free module");
        assert_eq!(analysis.contracts, [STRING_CONCAT_CONTRACT]);
        // A literal named at the wrong segment is not confirmed.
        let mut wrong = plans.clone();
        wrong.types.str_segs[1].1 = 0;
        let analysis =
            analyze(&fixture("hello"), &wrong, crate::format::TARGET_WASM_GC).expect("analyzes");
        assert_eq!(analysis.certified_names(), ["greet"]);
    }

    #[test]
    fn the_rendered_plans_are_the_lean_grammar_terms() {
        assert_eq!(
            sum_to().lean(),
            "{ sig := ⟨[.int], .int⟩, nslots := 1, locals := [.int],\n    body := (.ifThenElse (.binOp .lte (.local 0) (.literal (.int 0))) (.literal (.int 0)) (.binOp .add (.local 0) (.call (.fn 1) [(.binOp .sub (.local 0) (.literal (.int 1)))]))) }"
        );
        let m = PlanExpr::Match(
            Box::new(l(0)),
            vec![(PlanPat::LitInt(-3), k(1)), (PlanPat::Wild, k(2))],
        );
        assert_eq!(
            m.lean(),
            "(.match_ (.local 0) (.cons (.litInt (-3)) (.literal (.int 1)) (.cons .wild (.literal (.int 2)) .nil)))"
        );
    }

    #[test]
    fn twin_termination_matches_the_wall_check() {
        let p = sum_to();
        assert_eq!(check_term_group(&[(1, &p)]), Some(false));
        let wild = rec(
            1,
            k(0),
            bin(
                PlanBinOp::Mul,
                k(3),
                PlanExpr::Call(PlanCallee::Fn(1), vec![desc()]),
            ),
        );
        assert_eq!(check_term_group(&[(1, &wild)]), Some(true));
        let by_two = rec(
            1,
            k(0),
            PlanExpr::Call(PlanCallee::Fn(1), vec![bin(PlanBinOp::Sub, l(0), k(2))]),
        );
        assert_eq!(check_term_group(&[(1, &by_two)]), None);
        let no_call = rec(1, k(0), l(0));
        assert_eq!(check_term_group(&[(1, &no_call)]), None);
    }
}
