// ---- producer-side twins of the wall's plan functions ----------------------
//
// Rust ports of `Grammar.tyOf`, `GrammarLower.lowerB`/`codeEntryBytes`,
// `TypeTable.typeTableConfirmed`, `GrammarTotal.checkTermGroup` and the
// `ClaimAxes` report functions. They are PRODUCER-ONLY: they decide which
// functions the producer offers (a plan whose lowering is not its code entry
// is declined per function instead of failing the whole package in Lean) and
// fill the report fields the checker witness pins. The wall re-derives every
// one of them; a disagreement fails closed there.

/// `TypeTable.absent k`: an index outside the u32 space, which every encoder
/// rejects.
fn absent(k: u64) -> u64 {
    4_294_967_296 + k
}

fn idx_or(k: u64, v: Option<u32>) -> u64 {
    v.map_or(absent(k), u64::from)
}

/// `Grammar.MCtx`, as `TypeTable.mctxOf` builds it.
struct MCtx<'a> {
    carrier: u64,
    box_: u64,
    add: u64,
    sub: u64,
    mul: u64,
    neg: u64,
    cmp: u64,
    eq: u64,
    mag: u64,
    str_: u64,
    str_vec: u64,
    concat: u64,
    streq: u64,
    to_index: u64,
    divmod: u64,
    /// `__aint_to_i64_sat` (`MCtx.toI64Sat`).
    to_i64_sat: u64,
    /// The packed `Bytes` array (`MCtx.bytesArr`) and `__aint_to_i64_checked`
    /// (`MCtx.toI64Chk`).
    bytes_arr: u64,
    to_i64_chk: u64,
    tt: &'a PlanTypeTable,
    sigs: HashMap<u32, (Vec<PlanTy>, PlanTy)>,
}

impl<'a> MCtx<'a> {
    fn new(
        roles: Option<&HostRoles>,
        strings: &StringHostRoles,
        tt: &'a PlanTypeTable,
        fns: &[(u32, &FnPlan)],
    ) -> Self {
        let role = |pick: fn(&HostRoles) -> Option<u32>| roles.and_then(pick);
        let string_role = |r: StringHostRole| strings.iter().find(|x| x.1 == r).map(|x| x.0);
        let mut sigs = HashMap::new();
        for (f, p) in fns {
            sigs.entry(*f)
                .or_insert_with(|| (p.params.clone(), p.ret.clone()));
        }
        MCtx {
            carrier: idx_or(0, tt.carrier),
            box_: idx_or(1, role(|r| r.box_idx)),
            add: idx_or(2, role(|r| r.add_idx)),
            sub: idx_or(3, role(|r| r.sub_idx)),
            mul: idx_or(4, role(|r| r.mul_idx)),
            neg: absent(5),
            cmp: idx_or(6, role(|r| r.cmp_idx)),
            eq: idx_or(7, role(|r| r.eq_idx)),
            mag: idx_or(13, tt.mag),
            str_: idx_or(14, tt.str_),
            str_vec: idx_or(16, tt.str_vec),
            concat: idx_or(17, string_role(StringHostRole::Concat)),
            streq: idx_or(18, string_role(StringHostRole::Eq)),
            to_index: idx_or(19, role(|r| r.to_index_idx)),
            divmod: idx_or(23, role(|r| r.divmod_idx)),
            to_i64_sat: idx_or(24, tt.int_sat),
            bytes_arr: idx_or(25, tt.bytes_arr),
            to_i64_chk: idx_or(26, tt.int_chk),
            tt,
            sigs,
        }
    }

    fn record(&self, tid: u32) -> Option<&PlanRecordDecl> {
        self.tt.records.iter().find(|r| r.tid == tid)
    }

    fn sum(&self, tid: u32) -> Option<&PlanSumDecl> {
        self.tt.sums.iter().find(|s| s.tid == tid)
    }

    fn rec_fields(&self, tid: u32) -> Option<&[PlanTy]> {
        self.record(tid).map(|r| r.fields.as_slice())
    }

    fn struct_of(&self, tid: u32) -> u64 {
        idx_or(8, self.record(tid).map(|r| r.struct_idx))
    }

    fn ctor_fields(&self, tid: u32, c: u32) -> Option<&[PlanTy]> {
        self.sum(tid)?.ctors.get(c as usize).map(|x| x.1.as_slice())
    }

    fn ctor_struct(&self, tid: u32, c: u32) -> u64 {
        idx_or(
            9,
            self.sum(tid)
                .and_then(|s| s.ctors.get(c as usize))
                .map(|x| x.0),
        )
    }

    fn sum_root(&self, tid: u32) -> u64 {
        idx_or(10, self.sum(tid).map(|s| s.root))
    }

    fn opt_struct(&self, t: &PlanTy) -> u64 {
        idx_or(11, self.tt.options.iter().find(|o| &o.0 == t).map(|o| o.1))
    }

    fn res_struct(&self, t: &PlanTy, e: &PlanTy) -> u64 {
        idx_or(
            12,
            self.tt
                .results
                .iter()
                .find(|r| &r.0 == t && &r.1 == e)
                .map(|r| r.2),
        )
    }

    fn str_seg(&self, bytes: &[u8]) -> u64 {
        idx_or(
            15,
            self.tt.str_segs.iter().find(|s| s.0 == bytes).map(|s| s.1),
        )
    }

    fn vec_struct(&self, t: &PlanTy) -> u64 {
        idx_or(20, self.tt.vecs.iter().find(|v| &v.0 == t).map(|v| v.1))
    }

    fn list_struct(&self, t: &PlanTy) -> u64 {
        idx_or(21, self.tt.lists.iter().find(|l| &l.0 == t).map(|l| l.1))
    }

    /// `MCtx.listCons`: the declared cons helper of `List<t>`.
    fn list_cons(&self, t: &PlanTy) -> Option<u32> {
        self.tt.list_cons.iter().find(|l| &l.0 == t).map(|l| l.1)
    }

    fn opaque_struct(&self, tid: u32) -> u64 {
        idx_or(22, self.tt.opaques.iter().find(|o| o.0 == tid).map(|o| o.1))
    }

    fn sum_ok(&self, tid: u32) -> bool {
        let Some(s) = self.sum(tid) else {
            return false;
        };
        let cs = &s.ctors;
        let newtype = cs.len() == 1 && cs[0].1.len() == 1;
        if newtype {
            return false;
        }
        (0..cs.len()).all(|a| {
            (0..cs.len()).all(|b| {
                a == b || self.ctor_struct(tid, a as u32) != self.ctor_struct(tid, b as u32)
            })
        })
    }

    fn arith_idx(&self, op: PlanBinOp) -> u64 {
        match op {
            PlanBinOp::Add => self.add,
            PlanBinOp::Sub => self.sub,
            _ => self.mul,
        }
    }
}

fn has_default(t: &PlanTy) -> bool {
    matches!(
        t,
        PlanTy::Int
            | PlanTy::Bool
            | PlanTy::Record(_)
            | PlanTy::Sum(_)
            | PlanTy::Option(_)
            | PlanTy::Result(_, _)
            | PlanTy::Str
            | PlanTy::Float
            | PlanTy::List(_)
            | PlanTy::Vec(_)
            | PlanTy::Bytes
    )
}

type Gamma = BTreeMap<u32, PlanTy>;

fn upd(g: &Gamma, b: u32, t: PlanTy) -> Gamma {
    let mut g = g.clone();
    g.insert(b, t);
    g
}

fn bind_tys(n: u32, g: &Gamma, bs: &[u32], ts: &[PlanTy]) -> Option<Gamma> {
    if bs.len() != ts.len() {
        return None;
    }
    let mut g = g.clone();
    for (b, t) in bs.iter().zip(ts) {
        if *b == PLAN_NO_SLOT {
            continue;
        }
        if *b < n && !g.contains_key(b) {
            g.insert(*b, t.clone());
        } else {
            return None;
        }
    }
    Some(g)
}

fn bind_one(n: u32, g: &Gamma, b: u32, t: &PlanTy) -> Option<Gamma> {
    bind_tys(n, g, &[b], std::slice::from_ref(t))
}

fn opt_pick(p1: &PlanPat, p2: &PlanPat) -> Option<(bool, u32)> {
    match (p1, p2) {
        (PlanPat::Ctor(PlanCtor::Some, b), PlanPat::Ctor(PlanCtor::None, n))
            if b.len() == 1 && n.is_empty() =>
        {
            Some((false, b[0]))
        }
        (PlanPat::Ctor(PlanCtor::Some, b), PlanPat::Wild) if b.len() == 1 => Some((false, b[0])),
        (PlanPat::Ctor(PlanCtor::None, n), PlanPat::Ctor(PlanCtor::Some, b))
            if b.len() == 1 && n.is_empty() =>
        {
            Some((true, b[0]))
        }
        (PlanPat::Ctor(PlanCtor::None, n), PlanPat::Wild) if n.is_empty() => {
            Some((true, PLAN_NO_SLOT))
        }
        _ => None,
    }
}

fn res_pick(p1: &PlanPat, p2: &PlanPat) -> Option<(bool, u32, u32)> {
    match (p1, p2) {
        (PlanPat::Ctor(PlanCtor::Ok, a), PlanPat::Ctor(PlanCtor::Err, b))
            if a.len() == 1 && b.len() == 1 =>
        {
            Some((false, a[0], b[0]))
        }
        (PlanPat::Ctor(PlanCtor::Ok, a), PlanPat::Wild) if a.len() == 1 => {
            Some((false, a[0], PLAN_NO_SLOT))
        }
        (PlanPat::Ctor(PlanCtor::Err, b), PlanPat::Ctor(PlanCtor::Ok, a))
            if a.len() == 1 && b.len() == 1 =>
        {
            Some((true, a[0], b[0]))
        }
        (PlanPat::Ctor(PlanCtor::Err, b), PlanPat::Wild) if b.len() == 1 => {
            Some((true, PLAN_NO_SLOT, b[0]))
        }
        _ => None,
    }
}

/// `Grammar.listPick`: `(swap, head, tail)`, `swap` when the cons arm is
/// first.
fn list_pick(p1: &PlanPat, p2: &PlanPat) -> Option<(bool, u32, u32)> {
    match (p1, p2) {
        (PlanPat::EmptyList, PlanPat::Cons(h, t)) => Some((false, *h, *t)),
        (PlanPat::EmptyList, PlanPat::Wild) => Some((false, PLAN_NO_SLOT, PLAN_NO_SLOT)),
        (PlanPat::Cons(h, t), PlanPat::EmptyList | PlanPat::Wild) => Some((true, *h, *t)),
        _ => None,
    }
}

/// `Grammar.vecGetOr?`.
fn vec_get_or(lb: PlanLazy, o: &PlanExpr, d: &PlanExpr) -> Option<(u32, u32)> {
    match (lb, o, d) {
        (
            PlanLazy::OptWithDefault,
            PlanExpr::Call(PlanCallee::Builtin(PlanBuiltin::VecGet), args),
            PlanExpr::Literal(_),
        ) => match args.as_slice() {
            [PlanExpr::Local(v), PlanExpr::Local(i)] => Some((*v, *i)),
            _ => None,
        },
        _ => None,
    }
}

/// `Grammar.divOr?`: the fused `Result.withDefault(Int.div/mod(a, b), <Int
/// literal>)`, as `(is_mod, a, b)`.
fn div_or<'e>(
    lb: PlanLazy,
    o: &'e PlanExpr,
    d: &PlanExpr,
) -> Option<(bool, &'e PlanExpr, &'e PlanExpr)> {
    match (lb, o, d) {
        (
            PlanLazy::ResWithDefault,
            PlanExpr::Call(PlanCallee::Builtin(b @ (PlanBuiltin::IntDiv | PlanBuiltin::IntMod)), args),
            PlanExpr::Literal(PlanLit::Int(_)),
        ) => match args.as_slice() {
            [a, bb] => Some((*b == PlanBuiltin::IntMod, a, bb)),
            _ => None,
        },
        _ => None,
    }
}

fn all_str(ts: &[PlanTy]) -> bool {
    !ts.is_empty() && ts.iter().all(|t| *t == PlanTy::Str)
}

/// `Ty.containsEq`.
fn contains_eq(t: &PlanTy) -> bool {
    matches!(t, PlanTy::Int | PlanTy::Str | PlanTy::Bool)
}

impl MCtx<'_> {
    /// `Grammar.builtinTy`.
    fn builtin_ty(&self, b: PlanBuiltin, ts: &[PlanTy]) -> Option<PlanTy> {
        use PlanListRole as R;
        let has = |r: R, t: &PlanTy| self.list_helper(r, t).is_some();
        let list = |t: &PlanTy| PlanTy::List(Box::new(t.clone()));
        match (b, ts) {
            (PlanBuiltin::BoolAnd | PlanBuiltin::BoolOr, [PlanTy::Bool, PlanTy::Bool]) => {
                Some(PlanTy::Bool)
            }
            (PlanBuiltin::BoolNot, [PlanTy::Bool]) => Some(PlanTy::Bool),
            (PlanBuiltin::ListPrepend, [t, PlanTy::List(t2)]) if t == t2.as_ref() => Some(list(t)),
            (PlanBuiltin::ListLen, [PlanTy::List(t)]) => has(R::Len, t).then_some(PlanTy::Int),
            (PlanBuiltin::ListReverse, [PlanTy::List(t)]) => has(R::Reverse, t).then(|| list(t)),
            (PlanBuiltin::ListConcat, [PlanTy::List(t), PlanTy::List(t2)]) => {
                (t == t2 && has(R::Concat, t)).then(|| list(t))
            }
            (PlanBuiltin::ListTake, [PlanTy::List(t), PlanTy::Int]) => {
                has(R::Take, t).then(|| list(t))
            }
            (PlanBuiltin::ListDrop, [PlanTy::List(t), PlanTy::Int]) => {
                has(R::Drop, t).then(|| list(t))
            }
            (PlanBuiltin::ListContains, [PlanTy::List(t), t2]) => {
                (t.as_ref() == t2 && contains_eq(t) && has(R::Contains, t)).then_some(PlanTy::Bool)
            }
            (PlanBuiltin::BytesOfList, [PlanTy::List(t)]) if **t == PlanTy::Int => self
                .bytes_helper(PlanBytesRole::Pack)
                .map(|_| PlanTy::Bytes),
            (PlanBuiltin::BytesValues, [PlanTy::Bytes]) => self
                .bytes_helper(PlanBytesRole::Unpack)
                .map(|_| list(&PlanTy::Int)),
            (PlanBuiltin::BytesLen, [PlanTy::Bytes]) => Some(PlanTy::Int),
            (PlanBuiltin::BytesConcat, [PlanTy::Bytes, PlanTy::Bytes]) => self
                .bytes_helper(PlanBytesRole::Concat)
                .map(|_| PlanTy::Bytes),
            (PlanBuiltin::BytesTake, [PlanTy::Bytes, PlanTy::Int]) => self
                .bytes_helper(PlanBytesRole::Take)
                .map(|_| PlanTy::Bytes),
            (PlanBuiltin::BytesDrop, [PlanTy::Bytes, PlanTy::Int]) => self
                .bytes_helper(PlanBytesRole::Drop)
                .map(|_| PlanTy::Bytes),
            _ => None,
        }
    }
}

fn lazy_ty(lb: PlanLazy, to: &PlanTy, td: &PlanTy) -> Option<PlanTy> {
    match (lb, to) {
        (PlanLazy::OptWithDefault, PlanTy::Option(t))
        | (PlanLazy::ResWithDefault, PlanTy::Result(t, _))
            if t.as_ref() == td && has_default(t) =>
        {
            Some(td.clone())
        }
        _ => None,
    }
}

fn is_arith(op: PlanBinOp) -> bool {
    matches!(op, PlanBinOp::Add | PlanBinOp::Sub | PlanBinOp::Mul)
}

fn is_equality(op: PlanBinOp) -> bool {
    matches!(op, PlanBinOp::Eq | PlanBinOp::Neq)
}

fn is_float_cmp(op: PlanBinOp) -> bool {
    matches!(
        op,
        PlanBinOp::Eq | PlanBinOp::Lt | PlanBinOp::Gt | PlanBinOp::Lte | PlanBinOp::Gte
    )
}

impl MCtx<'_> {
    fn ctor_ty(&self, c: PlanCtor, ty: &PlanTy, ts: &[PlanTy]) -> Option<PlanTy> {
        match (c, ty) {
            (PlanCtor::User(tid, k), PlanTy::Sum(tid2)) => {
                (tid == *tid2 && self.sum_ok(tid) && self.ctor_fields(tid, k) == Some(ts))
                    .then_some(PlanTy::Sum(tid))
            }
            (PlanCtor::Some, PlanTy::Option(t)) => {
                (ts == [t.as_ref().clone()] && has_default(t)).then(|| ty.clone())
            }
            (PlanCtor::None, PlanTy::Option(t)) => {
                (ts.is_empty() && has_default(t)).then(|| ty.clone())
            }
            (PlanCtor::Ok, PlanTy::Result(t, e)) => {
                (ts == [t.as_ref().clone()] && has_default(t) && has_default(e)).then(|| ty.clone())
            }
            (PlanCtor::Err, PlanTy::Result(t, e)) => {
                (ts == [e.as_ref().clone()] && has_default(t) && has_default(e)).then(|| ty.clone())
            }
            _ => None,
        }
    }

    fn tys_of(&self, n: u32, g: &Gamma, es: &[PlanExpr]) -> Option<Vec<PlanTy>> {
        es.iter().map(|e| self.ty_of(n, g, false, e)).collect()
    }

    /// `Grammar.tyOf`.
    fn ty_of(&self, n: u32, g: &Gamma, tail: bool, e: &PlanExpr) -> Option<PlanTy> {
        match e {
            PlanExpr::Literal(PlanLit::Int(_)) => Some(PlanTy::Int),
            PlanExpr::Literal(PlanLit::Bool(_)) => Some(PlanTy::Bool),
            PlanExpr::Literal(PlanLit::Float(_)) => Some(PlanTy::Float),
            PlanExpr::Literal(PlanLit::Str(_)) => Some(PlanTy::Str),
            PlanExpr::Local(i) => g.get(i).cloned(),
            PlanExpr::Let(b, v, body) => {
                if *b < n && !g.contains_key(b) {
                    let t = self.ty_of(n, g, false, v)?;
                    self.ty_of(n, &upd(g, *b, t), tail, body)
                } else {
                    None
                }
            }
            PlanExpr::Call(PlanCallee::Fn(f), args) | PlanExpr::TailCall(f, args) => {
                if matches!(e, PlanExpr::TailCall(..)) && !tail {
                    return None;
                }
                let (params, ret) = self.sigs.get(f)?;
                let ts = self.tys_of(n, g, args)?;
                (&ts == params).then(|| ret.clone())
            }
            PlanExpr::Call(PlanCallee::Builtin(b), args) => {
                self.builtin_ty(*b, &self.tys_of(n, g, args)?)
            }
            PlanExpr::Call(PlanCallee::Lazy(lb), args) => match args.as_slice() {
                [o, d] => match vec_get_or(*lb, o, d) {
                    Some((v, i)) => match (g.get(&v), g.get(&i), self.ty_of(n, g, false, d)) {
                        (Some(PlanTy::Vec(t)), Some(PlanTy::Int), Some(td)) if td == **t => {
                            Some(td)
                        }
                        _ => None,
                    },
                    None => match div_or(*lb, o, d) {
                        Some((_, a, b)) => (self.ty_of(n, g, false, a)? == PlanTy::Int
                            && self.ty_of(n, g, false, b)? == PlanTy::Int
                            && self.ty_of(n, g, false, d)? == PlanTy::Int)
                            .then_some(PlanTy::Int),
                        None => lazy_ty(
                            *lb,
                            &self.ty_of(n, g, false, o)?,
                            &self.ty_of(n, g, false, d)?,
                        ),
                    },
                },
                _ => None,
            },
            PlanExpr::Call(PlanCallee::Intrinsic(_), args) => match args.as_slice() {
                [a, PlanExpr::Literal(PlanLit::Int(k))] if *k != 0 => {
                    (self.ty_of(n, g, false, a)? == PlanTy::Int).then_some(PlanTy::Int)
                }
                _ => None,
            },
            PlanExpr::BinOp(op, l, r) => {
                match (self.ty_of(n, g, false, l)?, self.ty_of(n, g, false, r)?) {
                    (PlanTy::Int, PlanTy::Int) => Some(if is_arith(*op) {
                        PlanTy::Int
                    } else {
                        PlanTy::Bool
                    }),
                    (PlanTy::Bool, PlanTy::Bool) => is_equality(*op).then_some(PlanTy::Bool),
                    (PlanTy::Float, PlanTy::Float) => is_float_cmp(*op).then_some(PlanTy::Bool),
                    (PlanTy::Str, PlanTy::Str) => match op {
                        PlanBinOp::Add => Some(PlanTy::Str),
                        PlanBinOp::Eq | PlanBinOp::Neq => Some(PlanTy::Bool),
                        _ => None,
                    },
                    _ => None,
                }
            }
            PlanExpr::Neg(x) => (self.ty_of(n, g, false, x)? == PlanTy::Int).then_some(PlanTy::Int),
            PlanExpr::If(c, t, el) => {
                match (
                    self.ty_of(n, g, false, c)?,
                    self.ty_of(n, g, tail, t)?,
                    self.ty_of(n, g, tail, el)?,
                ) {
                    (PlanTy::Bool, a, b) if a == b => Some(a),
                    _ => None,
                }
            }
            PlanExpr::RecordCreate(tid, fs) => {
                let fts = self.rec_fields(*tid)?;
                let ts = self.tys_of(n, g, fs)?;
                (fts.len() >= 2 && ts == fts).then_some(PlanTy::Record(*tid))
            }
            PlanExpr::Project(tid, i, base) => {
                match (self.ty_of(n, g, false, base)?, self.rec_fields(*tid)) {
                    (PlanTy::Record(t2), Some(fts)) if t2 == *tid && fts.len() >= 2 => {
                        fts.get(*i as usize).cloned()
                    }
                    _ => None,
                }
            }
            PlanExpr::Construct(c, ty, args) => self.ctor_ty(*c, ty, &self.tys_of(n, g, args)?),
            PlanExpr::Interp(parts) => all_str(&self.tys_of(n, g, parts)?).then_some(PlanTy::Str),
            PlanExpr::List(t, items) => {
                let lt = PlanTy::List(Box::new(t.clone()));
                if items.is_empty() {
                    return Some(lt);
                }
                let f = self.list_cons(t)?;
                let ts = self.tys_of(n, g, items)?;
                let sig_ok = self.sigs.get(&f) == Some(&(vec![t.clone(), lt.clone()], lt.clone()));
                (sig_ok && ts.iter().all(|x| x == t)).then_some(lt)
            }
            PlanExpr::Match(s, arms) => match self.ty_of(n, g, false, s)? {
                PlanTy::Int => {
                    matches!(arms.first(), Some((PlanPat::LitInt(_), _))).then_some(())?;
                    self.ty_int_arms(n, g, tail, arms)
                }
                PlanTy::Bool => self.ty_bool_arms(n, g, tail, arms),
                PlanTy::Option(t) => self.ty_opt_arms(n, g, tail, &t, arms),
                PlanTy::Result(t, er) => self.ty_res_arms(n, g, tail, &t, &er, arms),
                PlanTy::Sum(tid) => {
                    (self.sum_ok(tid) && self.var_exhaustive(tid, arms) && arms.len() >= 2)
                        .then_some(())?;
                    self.ty_var_arms(n, g, tail, tid, arms)
                }
                PlanTy::Str => self.ty_str_arms(n, g, tail, arms),
                PlanTy::Record(tid) => self.ty_tup_arms(n, g, tail, tid, arms),
                PlanTy::List(t) => self.ty_list_arms(n, g, tail, &t, arms),
                _ => None,
            },
        }
    }

    fn ty_int_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        let ((p, b), rest) = arms.split_first()?;
        match p {
            PlanPat::LitInt(_) => {
                let t = self.ty_of(n, g, tail, b)?;
                let t2 = self.ty_int_arms(n, g, tail, rest)?;
                (t == t2).then_some(t)
            }
            PlanPat::Wild if rest.is_empty() => self.ty_of(n, g, tail, b),
            PlanPat::Bind(s) if rest.is_empty() => {
                (*s < n && !g.contains_key(s) && *s != PLAN_NO_SLOT).then_some(())?;
                self.ty_of(n, &upd(g, *s, PlanTy::Int), tail, b)
            }
            _ => None,
        }
    }

    fn ty_bool_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        match arms {
            [(PlanPat::LitBool(v), t), (p, e)]
                if *p == PlanPat::LitBool(!v) || *p == PlanPat::Wild =>
            {
                let a = self.ty_of(n, g, tail, t)?;
                let b = self.ty_of(n, g, tail, e)?;
                (a == b).then_some(a)
            }
            _ => None,
        }
    }

    fn ty_opt_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        t: &PlanTy,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        let [(p1, b1), (p2, b2)] = arms else {
            return None;
        };
        let (swap, sb) = opt_pick(p1, p2)?;
        let gs = bind_one(n, g, sb, t)?;
        let (a, b) = if swap {
            (self.ty_of(n, &gs, tail, b2)?, self.ty_of(n, g, tail, b1)?)
        } else {
            (self.ty_of(n, &gs, tail, b1)?, self.ty_of(n, g, tail, b2)?)
        };
        (a == b).then_some(a)
    }

    fn ty_res_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        t: &PlanTy,
        er: &PlanTy,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        let [(p1, b1), (p2, b2)] = arms else {
            return None;
        };
        let (swap, ob, eb) = res_pick(p1, p2)?;
        let go = bind_one(n, g, ob, t)?;
        let ge = bind_one(n, g, eb, er)?;
        let (a, b) = if swap {
            (self.ty_of(n, &go, tail, b2)?, self.ty_of(n, &ge, tail, b1)?)
        } else {
            (self.ty_of(n, &go, tail, b1)?, self.ty_of(n, &ge, tail, b2)?)
        };
        (a == b).then_some(a)
    }

    fn covers(c: u32, arms: &[(PlanPat, PlanExpr)]) -> bool {
        for (p, _) in arms {
            match p {
                PlanPat::Wild => return true,
                PlanPat::Ctor(PlanCtor::User(_, c2), _) if *c2 == c => return true,
                _ => {}
            }
        }
        false
    }

    fn var_exhaustive(&self, tid: u32, arms: &[(PlanPat, PlanExpr)]) -> bool {
        match self.sum(tid) {
            Some(s) => (0..s.ctors.len() as u32).all(|c| Self::covers(c, arms)),
            None => false,
        }
    }

    fn var_arm_gamma(&self, n: u32, g: &Gamma, tid: u32, p: &PlanPat) -> Option<Gamma> {
        match p {
            PlanPat::Ctor(PlanCtor::User(t2, c), bs) if *t2 == tid => {
                bind_tys(n, g, bs, self.ctor_fields(tid, *c)?)
            }
            PlanPat::Wild => Some(g.clone()),
            _ => None,
        }
    }

    fn ty_var_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        tid: u32,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        match arms {
            [] => None,
            [(p, b)] => self.ty_of(n, &self.var_arm_gamma(n, g, tid, p)?, tail, b),
            [(p, b), rest @ ..] => {
                if *p == PlanPat::Wild {
                    return None;
                }
                let g2 = self.var_arm_gamma(n, g, tid, p)?;
                let a = self.ty_of(n, &g2, tail, b)?;
                let c = self.ty_var_arms(n, g, tail, tid, rest)?;
                (a == c).then_some(a)
            }
        }
    }

    fn ty_str_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        let ((p, b), rest) = arms.split_first()?;
        match p {
            PlanPat::LitStr(_) => {
                let t = self.ty_of(n, g, tail, b)?;
                let t2 = self.ty_str_arms(n, g, tail, rest)?;
                (t == t2).then_some(t)
            }
            PlanPat::Wild if rest.is_empty() => self.ty_of(n, g, tail, b),
            _ => None,
        }
    }

    fn ty_list_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        t: &PlanTy,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        let [(p1, b1), (p2, b2)] = arms else {
            return None;
        };
        let (swap, h, tl) = list_pick(p1, p2)?;
        let gc = bind_tys(n, g, &[h, tl], &[t.clone(), PlanTy::List(Box::new(t.clone()))])?;
        let (a, b) = if swap {
            (self.ty_of(n, &gc, tail, b1)?, self.ty_of(n, g, tail, b2)?)
        } else {
            (self.ty_of(n, g, tail, b1)?, self.ty_of(n, &gc, tail, b2)?)
        };
        (a == b).then_some(a)
    }

    fn ty_tup_arms(
        &self,
        n: u32,
        g: &Gamma,
        tail: bool,
        tid: u32,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Option<PlanTy> {
        let [(PlanPat::Tuple(bs), b)] = arms else {
            return None;
        };
        let fts = self.rec_fields(tid)?;
        (fts.len() >= 2 && bs.iter().any(|x| *x != PLAN_NO_SLOT)).then_some(())?;
        self.ty_of(n, &bind_tys(n, g, bs, fts)?, tail, b)
    }
}

fn params_gamma(ps: &[PlanTy]) -> Gamma {
    ps.iter()
        .enumerate()
        .map(|(i, t)| (i as u32, t.clone()))
        .collect()
}

/// `Grammar.planTyped`.
fn plan_typed(m: &MCtx<'_>, p: &FnPlan) -> bool {
    let np = p.params.len();
    np <= p.nslots as usize
        && (p.nslots as usize) <= np + p.locals.len()
        && m.ty_of(p.nslots, &params_gamma(&p.params), true, &p.body) == Some(p.ret.clone())
}

// ---- `TypeTable.declsWellFormed`: no vacuous obligation ----

/// `TypeTable.noEqref`.
fn no_eqref(t: &PlanTy) -> bool {
    match t {
        PlanTy::Eqref => false,
        PlanTy::Option(x) | PlanTy::Vec(x) | PlanTy::List(x) => no_eqref(x),
        PlanTy::Result(x, e) => no_eqref(x) && no_eqref(e),
        _ => true,
    }
}

/// `TypeTable.planEqrefOk`: `eqref` only as the subject-scratch local.
fn plan_eqref_ok(p: &FnPlan) -> bool {
    let scratch = (p.nslots as usize).saturating_sub(p.params.len());
    p.params.iter().all(no_eqref)
        && no_eqref(&p.ret)
        && p.locals
            .iter()
            .enumerate()
            .all(|(i, t)| no_eqref(t) || (i == scratch && *t == PlanTy::Eqref))
}

/// `TypeTable.inhabTy`.
fn inhab_ty(r: &BTreeSet<u32>, s: &BTreeSet<u32>, t: &PlanTy) -> bool {
    match t {
        PlanTy::Int
        | PlanTy::Bool
        | PlanTy::Float
        | PlanTy::Str
        | PlanTy::Opaque(_)
        | PlanTy::Option(_)
        | PlanTy::List(_)
        | PlanTy::Vec(_)
        | PlanTy::Bytes => true,
        PlanTy::Result(x, e) => inhab_ty(r, s, x) || inhab_ty(r, s, e),
        PlanTy::Record(tid) => r.contains(tid),
        PlanTy::Sum(tid) => s.contains(tid),
        PlanTy::Eqref => false,
    }
}

/// `TypeTable.inhabSets`: the record and sum ids with a finite value (the
/// least fixpoint of `inhabStep`).
fn inhab_sets(tt: &PlanTypeTable) -> (BTreeSet<u32>, BTreeSet<u32>) {
    let mut r: BTreeSet<u32> = BTreeSet::new();
    let mut s: BTreeSet<u32> = BTreeSet::new();
    for _ in 0..=(tt.records.len() + tt.sums.len()) {
        let nr: BTreeSet<u32> = tt
            .records
            .iter()
            .filter(|d| {
                tt.records
                    .iter()
                    .find(|x| x.tid == d.tid)
                    .is_some_and(|x| x.fields.iter().all(|f| inhab_ty(&r, &s, f)))
            })
            .map(|d| d.tid)
            .collect();
        let ns: BTreeSet<u32> = tt
            .sums
            .iter()
            .filter(|d| {
                tt.sums.iter().find(|x| x.tid == d.tid).is_some_and(|x| {
                    x.ctors
                        .iter()
                        .any(|c| c.1.iter().all(|f| inhab_ty(&r, &s, f)))
                })
            })
            .map(|d| d.tid)
            .collect();
        r = nr;
        s = ns;
    }
    (r, s)
}

/// `TypeTable.ntGrounded` from a record id: its chain of one-field records
/// ends at a type that is not one.
fn newtype_grounded(tt: &PlanTypeTable, tid: u32) -> bool {
    let mut cur = PlanTy::Record(tid);
    for _ in 0..=tt.records.len() {
        let PlanTy::Record(t) = cur else {
            return true;
        };
        match tt.records.iter().find(|r| r.tid == t) {
            Some(r) if r.fields.len() == 1 => cur = r.fields[0].clone(),
            _ => return true,
        }
    }
    false
}

/// `Grammar.LCtx`.
struct LCtx {
    n: u32,
    cmp: u32,
    subj: u32,
}

fn lctx(p: &FnPlan) -> LCtx {
    let at = (p.nslots as usize).checked_sub(p.params.len());
    if at.and_then(|i| p.locals.get(i)) == Some(&PlanTy::Eqref) {
        LCtx {
            n: p.nslots,
            cmp: p.nslots + 1,
            subj: p.nslots,
        }
    } else {
        LCtx {
            n: p.nslots,
            cmp: p.nslots,
            subj: p.nslots,
        }
    }
}

/// `CertPrelude.WInstr`, the admitted part.
#[derive(Clone, Debug, PartialEq)]
enum WI {
    LocalGet(u32),
    LocalSet(u32),
    I64Const(i64),
    I32Const(i64),
    Call(u64),
    ReturnCall(u64),
    StructNew(u64),
    StructGet(u64, u32),
    RefIsNull,
    I64Eqz,
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32GtS,
    I32LeS,
    I32GeS,
    I64Eq,
    I64Ne,
    I64LtS,
    I64GtS,
    I64LeS,
    I64GeS,
    I32And,
    I32Or,
    I32LtU,
    I64ExtendI32U,
    F64Const(u64),
    F64Eq,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    ArrayLen,
    ArrayGet(u64),
    ArrayNewFixed(u64, u32),
    RefTest(u64),
    RefCast(u64),
}

/// `GrammarLower.BI`.
#[derive(Clone, Debug, PartialEq)]
enum BI {
    Op(WI),
    If(Option<PlanTy>, Vec<BI>, Vec<BI>),
    NullOf(u64),
    NewData(u64, u64),
    CastNull(u64),
}

fn flip(op: PlanBinOp) -> PlanBinOp {
    match op {
        PlanBinOp::Lt => PlanBinOp::Gt,
        PlanBinOp::Gt => PlanBinOp::Lt,
        PlanBinOp::Lte => PlanBinOp::Gte,
        PlanBinOp::Gte => PlanBinOp::Lte,
        other => other,
    }
}

fn small_cmp(op: PlanBinOp) -> WI {
    match op {
        PlanBinOp::Eq => WI::I64Eq,
        PlanBinOp::Neq => WI::I64Ne,
        PlanBinOp::Lt => WI::I64LtS,
        PlanBinOp::Gt => WI::I64GtS,
        PlanBinOp::Lte => WI::I64LeS,
        _ => WI::I64GeS,
    }
}

fn big_cmp(c: u64, s: u32, op: PlanBinOp) -> Vec<WI> {
    match op {
        PlanBinOp::Lt | PlanBinOp::Lte => vec![
            WI::LocalGet(s),
            WI::StructGet(c, 2),
            WI::I32Const(0),
            WI::I32LtS,
        ],
        PlanBinOp::Gt | PlanBinOp::Gte => vec![
            WI::LocalGet(s),
            WI::StructGet(c, 2),
            WI::I32Const(0),
            WI::I32GtS,
        ],
        PlanBinOp::Eq => vec![WI::I32Const(0)],
        PlanBinOp::Neq => vec![WI::I32Const(1)],
        _ => vec![],
    }
}

fn cmp_arm(c: u64, s: u32, op: PlanBinOp, k: i64) -> Vec<BI> {
    vec![
        BI::Op(WI::LocalGet(s)),
        BI::Op(WI::StructGet(c, 1)),
        BI::Op(WI::RefIsNull),
        BI::If(
            Some(PlanTy::Bool),
            vec![
                BI::Op(WI::LocalGet(s)),
                BI::Op(WI::StructGet(c, 0)),
                BI::Op(WI::I64Const(k)),
                BI::Op(small_cmp(op)),
            ],
            big_cmp(c, s, op).into_iter().map(BI::Op).collect(),
        ),
    ]
}

fn ops(is: Vec<WI>) -> Vec<BI> {
    is.into_iter().map(BI::Op).collect()
}

impl MCtx<'_> {
    fn int_cmp_tail(&self, op: PlanBinOp) -> Vec<WI> {
        match op {
            PlanBinOp::Eq => vec![WI::Call(self.eq)],
            PlanBinOp::Neq => vec![WI::Call(self.eq), WI::I32Eqz],
            PlanBinOp::Lt => vec![WI::Call(self.cmp), WI::I32Const(0), WI::I32LtS],
            PlanBinOp::Gt => vec![WI::Call(self.cmp), WI::I32Const(0), WI::I32GtS],
            PlanBinOp::Lte => vec![WI::Call(self.cmp), WI::I32Const(0), WI::I32LeS],
            _ => vec![WI::Call(self.cmp), WI::I32Const(0), WI::I32GeS],
        }
    }

    fn str_lit(&self, bytes: &[u8]) -> Vec<BI> {
        vec![
            BI::Op(WI::I32Const(0)),
            BI::Op(WI::I32Const(bytes.len() as i64)),
            BI::NewData(self.str_, self.str_seg(bytes)),
        ]
    }

    fn concat(&self, n: usize) -> Vec<BI> {
        vec![
            BI::Op(WI::ArrayNewFixed(self.str_vec, n as u32)),
            BI::Op(WI::Call(self.concat)),
        ]
    }

    fn dflt(&self, t: &PlanTy) -> Vec<BI> {
        match t {
            PlanTy::Int => vec![
                BI::Op(WI::I64Const(0)),
                BI::NullOf(self.mag),
                BI::Op(WI::I32Const(0)),
                BI::Op(WI::StructNew(self.carrier)),
            ],
            PlanTy::Bool => vec![BI::Op(WI::I32Const(0))],
            PlanTy::Record(tid) => vec![BI::NullOf(self.struct_of(*tid))],
            PlanTy::Sum(tid) => vec![BI::NullOf(self.sum_root(*tid))],
            PlanTy::Option(x) => vec![BI::NullOf(self.opt_struct(x))],
            PlanTy::Result(x, e) => vec![BI::NullOf(self.res_struct(x, e))],
            PlanTy::Str => vec![BI::NullOf(self.str_)],
            PlanTy::Float => vec![BI::Op(WI::F64Const(0))],
            PlanTy::List(x) => vec![BI::NullOf(self.list_struct(x))],
            PlanTy::Vec(x) => vec![BI::NullOf(self.vec_struct(x))],
            PlanTy::Bytes => vec![BI::NullOf(self.bytes_arr)],
            _ => vec![],
        }
    }

    fn lower_args(&self, x: &LCtx, g: &Gamma, es: &[PlanExpr]) -> Vec<BI> {
        es.iter().flat_map(|e| self.lower(x, g, false, e)).collect()
    }

    /// `GrammarLower.lowerB`.
    fn lower(&self, x: &LCtx, g: &Gamma, tail: bool, e: &PlanExpr) -> Vec<BI> {
        let n = x.n;
        match e {
            PlanExpr::Literal(PlanLit::Int(k)) => ops(vec![WI::I64Const(*k), WI::Call(self.box_)]),
            PlanExpr::Literal(PlanLit::Bool(b)) => ops(vec![WI::I32Const(i64::from(*b))]),
            PlanExpr::Literal(PlanLit::Float(bits)) => ops(vec![WI::F64Const(*bits)]),
            PlanExpr::Literal(PlanLit::Str(bytes)) => self.str_lit(bytes),
            PlanExpr::Local(i) => ops(vec![WI::LocalGet(*i)]),
            PlanExpr::Let(b, v, body) => {
                let mut out = self.lower(x, g, false, v);
                out.push(BI::Op(WI::LocalSet(*b)));
                let g2 = match self.ty_of(n, g, false, v) {
                    Some(t) => upd(g, *b, t),
                    None => g.clone(),
                };
                out.extend(self.lower(x, &g2, tail, body));
                out
            }
            PlanExpr::Call(PlanCallee::Fn(f), args) => {
                let mut out = self.lower_args(x, g, args);
                out.push(BI::Op(WI::Call(u64::from(*f))));
                out
            }
            PlanExpr::Call(PlanCallee::Builtin(b), args) => {
                let mut out = self.lower_args(x, g, args);
                match (b, self.tys_of(n, g, args).as_deref()) {
                    (PlanBuiltin::BoolAnd, _) => out.push(BI::Op(WI::I32And)),
                    (PlanBuiltin::BoolOr, _) => out.push(BI::Op(WI::I32Or)),
                    (PlanBuiltin::BoolNot, _) => out.push(BI::Op(WI::I32Eqz)),
                    (PlanBuiltin::ListPrepend, Some([_, PlanTy::List(t)])) => {
                        out.push(BI::Op(WI::StructNew(self.list_struct(t))))
                    }
                    (PlanBuiltin::ListLen, Some([PlanTy::List(t)])) => {
                        out.extend(self.helper_call(PlanListRole::Len, t));
                        out.push(BI::Op(WI::Call(self.box_)));
                    }
                    (PlanBuiltin::ListReverse, Some([PlanTy::List(t)])) => {
                        out.extend(self.helper_call(PlanListRole::Reverse, t))
                    }
                    (PlanBuiltin::ListConcat, Some([PlanTy::List(t), _])) => {
                        out.extend(self.helper_call(PlanListRole::Concat, t))
                    }
                    (PlanBuiltin::ListTake, Some([PlanTy::List(t), _])) => {
                        out.push(BI::Op(WI::Call(self.to_i64_sat)));
                        out.extend(self.helper_call(PlanListRole::Take, t));
                    }
                    (PlanBuiltin::ListDrop, Some([PlanTy::List(t), _])) => {
                        out.push(BI::Op(WI::Call(self.to_i64_sat)));
                        out.extend(self.helper_call(PlanListRole::Drop, t));
                    }
                    (PlanBuiltin::ListContains, Some([PlanTy::List(t), _])) => {
                        out.extend(self.helper_call(PlanListRole::Contains, t))
                    }
                    (PlanBuiltin::BytesOfList, _) => {
                        out.extend(self.bytes_call(PlanBytesRole::Pack))
                    }
                    (PlanBuiltin::BytesValues, _) => {
                        out.extend(self.bytes_call(PlanBytesRole::Unpack))
                    }
                    (PlanBuiltin::BytesLen, _) => out.extend(ops(vec![
                        WI::ArrayLen,
                        WI::I64ExtendI32U,
                        WI::Call(self.box_),
                    ])),
                    (PlanBuiltin::BytesConcat, _) => {
                        out.extend(self.bytes_call(PlanBytesRole::Concat))
                    }
                    (PlanBuiltin::BytesTake, _) => {
                        out.push(BI::Op(WI::Call(self.to_i64_sat)));
                        out.extend(self.bytes_call(PlanBytesRole::Take));
                    }
                    (PlanBuiltin::BytesDrop, _) => {
                        out.push(BI::Op(WI::Call(self.to_i64_sat)));
                        out.extend(self.bytes_call(PlanBytesRole::Drop));
                    }
                    _ => {}
                }
                out
            }
            PlanExpr::Call(PlanCallee::Intrinsic(i), args) => {
                let mut out = self.lower_args(x, g, args);
                out.extend(ops(vec![
                    WI::I32Const(i64::from(*i == PlanIntrinsic::IntModEuclid)),
                    WI::Call(self.divmod),
                ]));
                out
            }
            PlanExpr::TailCall(f, args) => {
                let mut out = self.lower_args(x, g, args);
                out.push(BI::Op(WI::ReturnCall(u64::from(*f))));
                out
            }
            PlanExpr::BinOp(op, l, r) => {
                let lt = self.ty_of(n, g, false, l);
                match lt {
                    Some(PlanTy::Bool) => {
                        let mut out = self.lower(x, g, false, l);
                        out.extend(self.lower(x, g, false, r));
                        out.push(BI::Op(if *op == PlanBinOp::Eq {
                            WI::I32Eq
                        } else {
                            WI::I32Ne
                        }));
                        out
                    }
                    Some(PlanTy::Float) => {
                        let mut out = self.lower(x, g, false, l);
                        out.extend(self.lower(x, g, false, r));
                        out.push(BI::Op(match op {
                            PlanBinOp::Eq => WI::F64Eq,
                            PlanBinOp::Lt => WI::F64Lt,
                            PlanBinOp::Gt => WI::F64Gt,
                            PlanBinOp::Lte => WI::F64Le,
                            _ => WI::F64Ge,
                        }));
                        out
                    }
                    Some(PlanTy::Str) => {
                        let mut out = self.lower(x, g, false, l);
                        out.extend(self.lower(x, g, false, r));
                        match op {
                            PlanBinOp::Add => out.extend(self.concat(2)),
                            PlanBinOp::Eq => out.push(BI::Op(WI::Call(self.streq))),
                            _ => out.extend(ops(vec![WI::Call(self.streq), WI::I32Eqz])),
                        }
                        out
                    }
                    _ => {
                        if is_arith(*op) {
                            let mut out = self.lower(x, g, false, l);
                            out.extend(self.lower(x, g, false, r));
                            out.push(BI::Op(WI::Call(self.arith_idx(*op))));
                            return out;
                        }
                        let lit = |ex: &PlanExpr| match ex {
                            PlanExpr::Literal(PlanLit::Int(k)) => Some(*k),
                            _ => None,
                        };
                        let slot = |ex: &PlanExpr| match ex {
                            PlanExpr::Local(i) => Some(*i),
                            _ => None,
                        };
                        match (lit(l), lit(r)) {
                            (Some(k), _) => match slot(r) {
                                Some(i) => cmp_arm(self.carrier, i, flip(*op), k),
                                None => {
                                    let mut out = self.lower(x, g, false, r);
                                    out.push(BI::Op(WI::LocalSet(x.cmp)));
                                    out.extend(cmp_arm(self.carrier, x.cmp, flip(*op), k));
                                    out
                                }
                            },
                            (None, Some(k)) => match slot(l) {
                                Some(i) => cmp_arm(self.carrier, i, *op, k),
                                None => {
                                    let mut out = self.lower(x, g, false, l);
                                    out.push(BI::Op(WI::LocalSet(x.cmp)));
                                    out.extend(cmp_arm(self.carrier, x.cmp, *op, k));
                                    out
                                }
                            },
                            (None, None) => {
                                let mut out = self.lower(x, g, false, l);
                                out.extend(self.lower(x, g, false, r));
                                out.extend(ops(self.int_cmp_tail(*op)));
                                out
                            }
                        }
                    }
                }
            }
            PlanExpr::Neg(ex) => {
                let mut out = self.lower(x, g, false, ex);
                out.push(BI::Op(WI::Call(self.neg)));
                out
            }
            PlanExpr::If(c, t, el) => {
                let mut out = self.lower(x, g, false, c);
                out.push(BI::If(
                    self.ty_of(n, g, tail, t),
                    self.lower(x, g, tail, t),
                    self.lower(x, g, tail, el),
                ));
                out
            }
            PlanExpr::RecordCreate(tid, fs) => {
                let mut out = self.lower_args(x, g, fs);
                out.push(BI::Op(WI::StructNew(self.struct_of(*tid))));
                out
            }
            PlanExpr::Project(tid, i, base) => {
                let mut out = self.lower(x, g, false, base);
                out.push(BI::Op(WI::StructGet(self.struct_of(*tid), *i)));
                out
            }
            PlanExpr::Call(PlanCallee::Lazy(lb), args) => {
                let [o, d] = args.as_slice() else {
                    return vec![];
                };
                if let Some((v, i)) = vec_get_or(*lb, o, d) {
                    let Some(PlanTy::Vec(t)) = g.get(&v) else {
                        return vec![];
                    };
                    let ti = self.to_index;
                    let mut out = ops(vec![
                        WI::LocalGet(i),
                        WI::Call(ti),
                        WI::I32Const(0),
                        WI::I32GeS,
                        WI::LocalGet(i),
                        WI::Call(ti),
                        WI::LocalGet(v),
                        WI::ArrayLen,
                        WI::I32LtU,
                        WI::I32And,
                    ]);
                    out.push(BI::If(
                        Some(t.as_ref().clone()),
                        ops(vec![
                            WI::LocalGet(v),
                            WI::LocalGet(i),
                            WI::Call(ti),
                            WI::ArrayGet(self.vec_struct(t)),
                        ]),
                        self.lower(x, g, false, d),
                    ));
                    return out;
                }
                if let Some((is_mod, _, _)) = div_or(*lb, o, d) {
                    // `o`'s own lowering is its two operands.
                    let c = x.cmp;
                    let mut out = self.lower(x, g, false, o);
                    out.extend(self.lower(x, g, false, d));
                    out.extend(ops(vec![
                        WI::LocalSet(c + 3),
                        WI::LocalSet(c + 2),
                        WI::LocalSet(c + 1),
                        WI::LocalGet(c + 2),
                        WI::StructGet(self.carrier, 1),
                        WI::RefIsNull,
                        WI::LocalGet(c + 2),
                        WI::StructGet(self.carrier, 0),
                        WI::I64Eqz,
                        WI::I32And,
                    ]));
                    out.push(BI::If(
                        Some(PlanTy::Int),
                        ops(vec![WI::LocalGet(c + 3)]),
                        ops(vec![
                            WI::LocalGet(c + 1),
                            WI::LocalGet(c + 2),
                            WI::I32Const(i64::from(is_mod)),
                            WI::Call(self.divmod),
                        ]),
                    ));
                    return out;
                }
                let (st, payload_ty) = match (lb, self.ty_of(n, g, false, o)) {
                    (PlanLazy::OptWithDefault, Some(PlanTy::Option(t))) => {
                        (self.opt_struct(&t), *t)
                    }
                    (PlanLazy::ResWithDefault, Some(PlanTy::Result(t, er))) => {
                        (self.res_struct(&t, &er), *t)
                    }
                    _ => return vec![],
                };
                let mut out = self.lower(x, g, false, o);
                out.push(BI::Op(WI::LocalSet(x.subj)));
                out.extend(tag_test(x.subj, st));
                out.push(BI::If(
                    Some(payload_ty),
                    ops(vec![
                        WI::LocalGet(x.subj),
                        WI::RefCast(st),
                        WI::StructGet(st, 1),
                    ]),
                    self.lower(x, g, false, d),
                ));
                out
            }
            PlanExpr::Construct(c, ty, args) => match (c, ty) {
                (PlanCtor::User(tid, k), _) => {
                    let mut out = self.lower_args(x, g, args);
                    out.push(BI::Op(WI::StructNew(self.ctor_struct(*tid, *k))));
                    out
                }
                (PlanCtor::Some, PlanTy::Option(t)) => {
                    let mut out = ops(vec![WI::I32Const(1)]);
                    out.extend(self.lower_args(x, g, args));
                    out.push(BI::Op(WI::StructNew(self.opt_struct(t))));
                    out
                }
                (PlanCtor::None, PlanTy::Option(t)) => {
                    let mut out = ops(vec![WI::I32Const(0)]);
                    out.extend(self.dflt(t));
                    out.extend(self.lower_args(x, g, args));
                    out.push(BI::Op(WI::StructNew(self.opt_struct(t))));
                    out
                }
                (PlanCtor::Ok, PlanTy::Result(t, er)) => {
                    let mut out = ops(vec![WI::I32Const(1)]);
                    out.extend(self.lower_args(x, g, args));
                    out.extend(self.dflt(er));
                    out.push(BI::Op(WI::StructNew(self.res_struct(t, er))));
                    out
                }
                (PlanCtor::Err, PlanTy::Result(t, er)) => {
                    let mut out = ops(vec![WI::I32Const(0)]);
                    out.extend(self.dflt(t));
                    out.extend(self.lower_args(x, g, args));
                    out.push(BI::Op(WI::StructNew(self.res_struct(t, er))));
                    out
                }
                _ => vec![],
            },
            PlanExpr::Match(s, arms) => {
                let bt = self.ty_of(n, g, tail, e);
                let sc = self.lower(x, g, false, s);
                match self.ty_of(n, g, false, s) {
                    Some(PlanTy::Int) => self.lower_int_arms(x, g, tail, &sc, &bt, arms),
                    Some(PlanTy::Bool) => {
                        let mut out = sc;
                        if let [(PlanPat::LitBool(v), t), (_, el), ..] = arms.as_slice() {
                            let (a, b) = if *v { (t, el) } else { (el, t) };
                            out.push(BI::If(
                                bt,
                                self.lower(x, g, tail, a),
                                self.lower(x, g, tail, b),
                            ));
                        }
                        out
                    }
                    Some(PlanTy::Option(t)) => {
                        let mut out = sc;
                        out.push(BI::Op(WI::LocalSet(x.subj)));
                        if let [(p1, b1), (p2, b2), ..] = arms.as_slice()
                            && let Some((swap, sb)) = opt_pick(p1, p2)
                        {
                            let st = self.opt_struct(&t);
                            let (some_b, none_b) = if swap { (b2, b1) } else { (b1, b2) };
                            let gs = bind_one(n, g, sb, &t).unwrap_or_else(|| g.clone());
                            out.extend(tag_test(x.subj, st));
                            let mut then_b = bind_field(x.subj, st, 1, sb);
                            then_b.extend(self.lower(x, &gs, tail, some_b));
                            out.push(BI::If(bt, then_b, self.lower(x, g, tail, none_b)));
                        }
                        out
                    }
                    Some(PlanTy::Result(t, er)) => {
                        let mut out = sc;
                        out.push(BI::Op(WI::LocalSet(x.subj)));
                        if let [(p1, b1), (p2, b2), ..] = arms.as_slice()
                            && let Some((swap, ob, eb)) = res_pick(p1, p2)
                        {
                            let st = self.res_struct(&t, &er);
                            let (ok_b, err_b) = if swap { (b2, b1) } else { (b1, b2) };
                            let go = bind_one(n, g, ob, &t).unwrap_or_else(|| g.clone());
                            let ge = bind_one(n, g, eb, &er).unwrap_or_else(|| g.clone());
                            out.extend(tag_test(x.subj, st));
                            let mut then_b = bind_field(x.subj, st, 1, ob);
                            then_b.extend(self.lower(x, &go, tail, ok_b));
                            let mut else_b = bind_field(x.subj, st, 2, eb);
                            else_b.extend(self.lower(x, &ge, tail, err_b));
                            out.push(BI::If(bt, then_b, else_b));
                        }
                        out
                    }
                    Some(PlanTy::Sum(tid)) => {
                        let mut out = sc;
                        out.push(BI::Op(WI::LocalSet(x.subj)));
                        out.extend(self.lower_var_arms(x, g, tail, &bt, tid, arms));
                        out
                    }
                    Some(PlanTy::Str) => {
                        let mut out = sc;
                        out.push(BI::Op(WI::LocalSet(x.subj)));
                        out.extend(self.lower_str_arms(x, g, tail, &bt, arms));
                        out
                    }
                    Some(PlanTy::Record(tid)) => {
                        let mut out = sc;
                        out.push(BI::Op(WI::LocalSet(x.subj)));
                        if let [(PlanPat::Tuple(bs), b), ..] = arms.as_slice() {
                            out.extend(extract(x.subj, self.struct_of(tid), bs));
                            let g2 = self
                                .rec_fields(tid)
                                .and_then(|fts| bind_tys(n, g, bs, fts))
                                .unwrap_or_else(|| g.clone());
                            out.extend(self.lower(x, &g2, tail, b));
                        }
                        out
                    }
                    Some(PlanTy::List(t)) => {
                        let mut out = sc;
                        out.push(BI::Op(WI::LocalSet(x.subj)));
                        if let [(p1, b1), (p2, b2), ..] = arms.as_slice()
                            && let Some((swap, h, tl)) = list_pick(p1, p2)
                        {
                            let (empty_b, cons_b) = if swap { (b2, b1) } else { (b1, b2) };
                            let lt = PlanTy::List(t.clone());
                            let gc = bind_tys(n, g, &[h, tl], &[(*t).clone(), lt])
                                .unwrap_or_else(|| g.clone());
                            let mut else_b = extract(x.subj, self.list_struct(&t), &[h, tl]);
                            else_b.extend(self.lower(x, &gc, tail, cons_b));
                            out.extend(ops(vec![WI::LocalGet(x.subj), WI::RefIsNull]));
                            out.push(BI::If(bt, self.lower(x, g, tail, empty_b), else_b));
                        }
                        out
                    }
                    _ => vec![],
                }
            }
            PlanExpr::Interp(parts) => {
                let mut out = self.lower_args(x, g, parts);
                out.extend(self.concat(parts.len()));
                out
            }
            PlanExpr::List(t, items) => {
                if items.is_empty() {
                    return vec![BI::NullOf(self.list_struct(t))];
                }
                let Some(f) = self.list_cons(t) else {
                    return vec![];
                };
                let mut out = self.lower_args(x, g, items);
                out.push(BI::NullOf(self.list_struct(t)));
                out.extend(items.iter().map(|_| BI::Op(WI::Call(u64::from(f)))));
                out
            }
        }
    }

    fn lower_int_arms(
        &self,
        x: &LCtx,
        g: &Gamma,
        tail: bool,
        sc: &[BI],
        bt: &Option<PlanTy>,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Vec<BI> {
        let Some(((p, b), rest)) = arms.split_first() else {
            return vec![];
        };
        match p {
            PlanPat::LitInt(k) => {
                let mut out = sc.to_vec();
                out.extend(ops(vec![
                    WI::I64Const(*k),
                    WI::Call(self.box_),
                    WI::Call(self.eq),
                ]));
                out.push(BI::If(
                    bt.clone(),
                    self.lower(x, g, tail, b),
                    self.lower_int_arms(x, g, tail, sc, bt, rest),
                ));
                out
            }
            PlanPat::Wild => self.lower(x, g, tail, b),
            PlanPat::Bind(s) => {
                let mut out = sc.to_vec();
                out.push(BI::Op(WI::LocalSet(*s)));
                out.extend(self.lower(x, &upd(g, *s, PlanTy::Int), tail, b));
                out
            }
            _ => vec![],
        }
    }

    fn lower_var_arms(
        &self,
        x: &LCtx,
        g: &Gamma,
        tail: bool,
        bt: &Option<PlanTy>,
        tid: u32,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Vec<BI> {
        match arms {
            [] => vec![],
            [(p, b)] => match p {
                PlanPat::Ctor(PlanCtor::User(t2, c), bs) => {
                    let mut out = extract(x.subj, self.ctor_struct(*t2, *c), bs);
                    let g2 = self
                        .var_arm_gamma(x.n, g, tid, p)
                        .unwrap_or_else(|| g.clone());
                    out.extend(self.lower(x, &g2, tail, b));
                    out
                }
                _ => self.lower(x, g, tail, b),
            },
            [(p, b), rest @ ..] => match p {
                PlanPat::Ctor(PlanCtor::User(t2, c), bs) => {
                    let st = self.ctor_struct(*t2, *c);
                    let mut then_b = extract(x.subj, st, bs);
                    let g2 = self
                        .var_arm_gamma(x.n, g, tid, p)
                        .unwrap_or_else(|| g.clone());
                    then_b.extend(self.lower(x, &g2, tail, b));
                    vec![
                        BI::Op(WI::LocalGet(x.subj)),
                        BI::Op(WI::RefTest(st)),
                        BI::If(
                            bt.clone(),
                            then_b,
                            self.lower_var_arms(x, g, tail, bt, tid, rest),
                        ),
                    ]
                }
                PlanPat::Wild => self.lower(x, g, tail, b),
                _ => vec![],
            },
        }
    }

    fn lower_str_arms(
        &self,
        x: &LCtx,
        g: &Gamma,
        tail: bool,
        bt: &Option<PlanTy>,
        arms: &[(PlanPat, PlanExpr)],
    ) -> Vec<BI> {
        let Some(((p, b), rest)) = arms.split_first() else {
            return vec![];
        };
        match p {
            PlanPat::LitStr(k) => {
                let mut out = vec![BI::Op(WI::LocalGet(x.subj)), BI::CastNull(self.str_)];
                out.extend(self.str_lit(k));
                out.push(BI::Op(WI::Call(self.streq)));
                out.push(BI::If(
                    bt.clone(),
                    self.lower(x, g, tail, b),
                    self.lower_str_arms(x, g, tail, bt, rest),
                ));
                out
            }
            _ => self.lower(x, g, tail, b),
        }
    }
}

fn tag_test(ss: u32, idx: u64) -> Vec<BI> {
    ops(vec![
        WI::LocalGet(ss),
        WI::RefCast(idx),
        WI::StructGet(idx, 0),
        WI::I32Const(1),
        WI::I32Eq,
    ])
}

fn bind_field(ss: u32, idx: u64, i: u32, b: u32) -> Vec<BI> {
    if b == PLAN_NO_SLOT {
        vec![]
    } else {
        ops(vec![
            WI::LocalGet(ss),
            WI::RefCast(idx),
            WI::StructGet(idx, i),
            WI::LocalSet(b),
        ])
    }
}

fn extract(ss: u32, idx: u64, bs: &[u32]) -> Vec<BI> {
    bs.iter()
        .enumerate()
        .flat_map(|(i, b)| bind_field(ss, idx, i as u32, *b))
        .collect()
}

// ---- the byte image ----

fn uleb(v: u64, out: &mut Vec<u8>) -> Option<()> {
    if v >= 4_294_967_296 {
        return None;
    }
    let mut v = v;
    loop {
        let byte = (v & 0x7f) as u8;
        v >>= 7;
        if v == 0 {
            out.push(byte);
            return Some(());
        }
        out.push(byte | 0x80);
    }
}

fn sleb(v: i64, out: &mut Vec<u8>) {
    let mut v = v;
    loop {
        let byte = (v & 0x7f) as u8;
        v >>= 7;
        let done = (v == 0 && byte & 0x40 == 0) || (v == -1 && byte & 0x40 != 0);
        if done {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}

fn s33(idx: u64, out: &mut Vec<u8>) -> Option<()> {
    if idx >= 4_294_967_296 {
        return None;
    }
    sleb(idx as i64, out);
    Some(())
}

impl MCtx<'_> {
    /// `GrammarLower.valTy`.
    fn val_ty(&self, t: &PlanTy, out: &mut Vec<u8>) -> Option<()> {
        let heap = |i: u64, out: &mut Vec<u8>| {
            out.push(0x63);
            s33(i, out)
        };
        match t {
            PlanTy::Int => heap(self.carrier, out),
            PlanTy::Bool => {
                out.push(0x7f);
                Some(())
            }
            PlanTy::Record(tid) => heap(self.struct_of(*tid), out),
            PlanTy::Sum(tid) => heap(self.sum_root(*tid), out),
            PlanTy::Option(x) => heap(self.opt_struct(x), out),
            PlanTy::Result(x, e) => heap(self.res_struct(x, e), out),
            PlanTy::Eqref => {
                out.push(0x6d);
                Some(())
            }
            PlanTy::Float => {
                out.push(0x7c);
                Some(())
            }
            PlanTy::Str => heap(self.str_, out),
            PlanTy::Vec(x) => heap(self.vec_struct(x), out),
            PlanTy::List(x) => heap(self.list_struct(x), out),
            PlanTy::Opaque(tid) => heap(self.opaque_struct(*tid), out),
            PlanTy::Bytes => heap(self.bytes_arr, out),
        }
    }

    fn enc_w(&self, i: &WI, out: &mut Vec<u8>) -> Option<()> {
        match i {
            WI::LocalGet(x) => {
                out.push(0x20);
                uleb(u64::from(*x), out)?
            }
            WI::LocalSet(x) => {
                out.push(0x21);
                uleb(u64::from(*x), out)?
            }
            WI::I64Const(k) => {
                out.push(0x42);
                sleb(*k, out)
            }
            WI::I32Const(k) => {
                if *k < i64::from(i32::MIN) || *k > i64::from(i32::MAX) {
                    return None;
                }
                out.push(0x41);
                sleb(*k, out)
            }
            WI::Call(f) => {
                out.push(0x10);
                uleb(*f, out)?
            }
            WI::ReturnCall(f) => {
                out.push(0x12);
                uleb(*f, out)?
            }
            WI::StructNew(t) => {
                out.extend([0xfb, 0x00]);
                uleb(*t, out)?
            }
            WI::StructGet(t, fld) => {
                out.extend([0xfb, 0x02]);
                uleb(*t, out)?;
                uleb(u64::from(*fld), out)?
            }
            WI::RefIsNull => out.push(0xd1),
            WI::I64Eqz => out.push(0x50),
            WI::I32Eqz => out.push(0x45),
            WI::I32Eq => out.push(0x46),
            WI::I32Ne => out.push(0x47),
            WI::I32LtS => out.push(0x48),
            WI::I32GtS => out.push(0x4a),
            WI::I32LeS => out.push(0x4c),
            WI::I32GeS => out.push(0x4e),
            WI::I64Eq => out.push(0x51),
            WI::I64Ne => out.push(0x52),
            WI::I64LtS => out.push(0x53),
            WI::I64GtS => out.push(0x55),
            WI::I64LeS => out.push(0x57),
            WI::I64GeS => out.push(0x59),
            WI::I32And => out.push(0x71),
            WI::I32Or => out.push(0x72),
            WI::I32LtU => out.push(0x49),
            WI::I64ExtendI32U => out.push(0xad),
            WI::F64Const(bits) => {
                out.push(0x44);
                out.extend(bits.to_le_bytes())
            }
            WI::F64Eq => out.push(0x61),
            WI::F64Lt => out.push(0x63),
            WI::F64Gt => out.push(0x64),
            WI::F64Le => out.push(0x65),
            WI::F64Ge => out.push(0x66),
            WI::ArrayLen => out.extend([0xfb, 0x0f]),
            WI::ArrayGet(t) => {
                out.extend([0xfb, 0x0b]);
                uleb(*t, out)?
            }
            WI::ArrayNewFixed(t, n) => {
                out.extend([0xfb, 0x08]);
                uleb(*t, out)?;
                uleb(u64::from(*n), out)?
            }
            WI::RefTest(t) => {
                out.extend([0xfb, 0x14]);
                s33(*t, out)?
            }
            WI::RefCast(t) => {
                out.extend([0xfb, 0x16]);
                s33(*t, out)?
            }
        }
        Some(())
    }

    fn enc(&self, bs: &[BI], out: &mut Vec<u8>) -> Option<()> {
        for b in bs {
            match b {
                BI::Op(i) => self.enc_w(i, out)?,
                BI::If(bt, t, e) => {
                    out.push(0x04);
                    self.val_ty(bt.as_ref()?, out)?;
                    self.enc(t, out)?;
                    out.push(0x05);
                    self.enc(e, out)?;
                    out.push(0x0b);
                }
                BI::NullOf(ht) => {
                    out.push(0xd0);
                    s33(*ht, out)?
                }
                BI::NewData(ty, seg) => {
                    out.extend([0xfb, 0x09]);
                    uleb(*ty, out)?;
                    uleb(*seg, out)?
                }
                BI::CastNull(ht) => {
                    out.extend([0xfb, 0x17]);
                    s33(*ht, out)?
                }
            }
        }
        Some(())
    }

    fn lower_plan(&self, p: &FnPlan) -> Vec<BI> {
        self.lower(&lctx(p), &params_gamma(&p.params), true, &p.body)
    }

    /// `GrammarLower.codeEntryBytes`.
    fn code_entry_bytes(&self, p: &FnPlan) -> Option<Vec<u8>> {
        let mut entry = Vec::new();
        uleb(p.locals.len() as u64, &mut entry)?;
        for t in &p.locals {
            entry.push(0x01);
            self.val_ty(t, &mut entry)?;
        }
        self.enc(&self.lower_plan(p), &mut entry)?;
        entry.push(0x0b);
        let mut out = Vec::new();
        uleb(entry.len() as u64, &mut out)?;
        out.extend(entry);
        Some(out)
    }

    /// `TypeTable.valTyD` against the decoded value type.
    fn val_t(&self, t: &PlanTy) -> Option<ValT> {
        let r = |i: u64| (i < 4_294_967_296).then_some(ValT::RefNull(i as u32));
        match t {
            PlanTy::Int => r(self.carrier),
            PlanTy::Bool => Some(ValT::I32),
            PlanTy::Float => Some(ValT::F64),
            PlanTy::Eqref => Some(ValT::Eqref),
            PlanTy::Record(tid) => r(self.struct_of(*tid)),
            PlanTy::Sum(tid) => r(self.sum_root(*tid)),
            PlanTy::Option(x) => r(self.opt_struct(x)),
            PlanTy::Result(x, e) => r(self.res_struct(x, e)),
            PlanTy::Str => r(self.str_),
            PlanTy::Vec(x) => r(self.vec_struct(x)),
            PlanTy::List(x) => r(self.list_struct(x)),
            PlanTy::Opaque(tid) => r(self.opaque_struct(*tid)),
            PlanTy::Bytes => r(self.bytes_arr),
        }
    }
}

/// Every function index a lowered body calls (`ClaimAxes.wCallsL`).
fn lowered_calls(bs: &[BI], out: &mut Vec<u64>) {
    for b in bs {
        match b {
            BI::Op(WI::Call(f)) | BI::Op(WI::ReturnCall(f)) => out.push(*f),
            BI::If(_, t, e) => {
                lowered_calls(t, out);
                lowered_calls(e, out);
            }
            _ => {}
        }
    }
}

/// `AcceptedArtifact.callTargets`.
fn call_targets(e: &PlanExpr, out: &mut Vec<u32>) {
    match e {
        PlanExpr::Literal(_) | PlanExpr::Local(_) => {}
        PlanExpr::Let(_, v, b) => {
            call_targets(v, out);
            call_targets(b, out);
        }
        PlanExpr::Call(PlanCallee::Fn(f), args) | PlanExpr::TailCall(f, args) => {
            out.push(*f);
            args.iter().for_each(|a| call_targets(a, out));
        }
        PlanExpr::Call(_, args)
        | PlanExpr::RecordCreate(_, args)
        | PlanExpr::Construct(_, _, args)
        | PlanExpr::Interp(args)
        | PlanExpr::List(_, args) => args.iter().for_each(|a| call_targets(a, out)),
        PlanExpr::BinOp(_, l, r) => {
            call_targets(l, out);
            call_targets(r, out);
        }
        PlanExpr::Neg(x) | PlanExpr::Project(_, _, x) => call_targets(x, out),
        PlanExpr::If(c, t, el) => {
            call_targets(c, out);
            call_targets(t, out);
            call_targets(el, out);
        }
        PlanExpr::Match(s, arms) => {
            call_targets(s, out);
            arms.iter().for_each(|(_, b)| call_targets(b, out));
        }
    }
}

/// The element types of the non-empty list literals of a plan: each one
/// calls its type's cons helper (`MCtx.listCons`), which must therefore be
/// offered with the plan.
fn literal_list_types(e: &PlanExpr, out: &mut Vec<PlanTy>) {
    match e {
        PlanExpr::Literal(_) | PlanExpr::Local(_) => {}
        PlanExpr::Let(_, v, b) | PlanExpr::BinOp(_, v, b) => {
            literal_list_types(v, out);
            literal_list_types(b, out);
        }
        PlanExpr::List(t, items) => {
            if !items.is_empty() {
                out.push(t.clone());
            }
            items.iter().for_each(|a| literal_list_types(a, out));
        }
        PlanExpr::Call(_, args)
        | PlanExpr::TailCall(_, args)
        | PlanExpr::RecordCreate(_, args)
        | PlanExpr::Construct(_, _, args)
        | PlanExpr::Interp(args) => args.iter().for_each(|a| literal_list_types(a, out)),
        PlanExpr::Neg(x) | PlanExpr::Project(_, _, x) => literal_list_types(x, out),
        PlanExpr::If(c, t, el) => {
            literal_list_types(c, out);
            literal_list_types(t, out);
            literal_list_types(el, out);
        }
        PlanExpr::Match(s, arms) => {
            literal_list_types(s, out);
            arms.iter().for_each(|(_, b)| literal_list_types(b, out));
        }
    }
}

/// The functions a plan runs: its calls (`call_targets`) and the cons
/// helpers of its non-empty list literals.
fn plan_targets(tt: &PlanTypeTable, p: &FnPlan) -> Vec<u32> {
    let mut out = Vec::new();
    call_targets(&p.body, &mut out);
    let mut tys = Vec::new();
    literal_list_types(&p.body, &mut tys);
    for t in tys {
        if let Some((_, f)) = tt.list_cons.iter().find(|l| l.0 == t) {
            out.push(*f);
        }
    }
    out
}

/// `GrammarLower.exprLits`: every string literal a plan lowers to
/// `array.new_data`.
fn string_lits<'e>(e: &'e PlanExpr, out: &mut Vec<&'e [u8]>) {
    match e {
        PlanExpr::Literal(PlanLit::Str(b)) => out.push(b),
        PlanExpr::Literal(_) | PlanExpr::Local(_) => {}
        PlanExpr::Let(_, v, b) => {
            string_lits(v, out);
            string_lits(b, out);
        }
        PlanExpr::Call(_, args)
        | PlanExpr::TailCall(_, args)
        | PlanExpr::RecordCreate(_, args)
        | PlanExpr::Construct(_, _, args)
        | PlanExpr::Interp(args)
        | PlanExpr::List(_, args) => args.iter().for_each(|a| string_lits(a, out)),
        PlanExpr::BinOp(_, l, r) => {
            string_lits(l, out);
            string_lits(r, out);
        }
        PlanExpr::Neg(x) | PlanExpr::Project(_, _, x) => string_lits(x, out),
        PlanExpr::If(c, t, el) => {
            string_lits(c, out);
            string_lits(t, out);
            string_lits(el, out);
        }
        PlanExpr::Match(s, arms) => {
            string_lits(s, out);
            for (p, b) in arms {
                if let PlanPat::LitStr(k) = p {
                    out.push(k);
                }
                string_lits(b, out);
            }
        }
    }
}

// ---- totality (`GrammarTotal`) ----

fn is_descent(e: &PlanExpr) -> bool {
    matches!(e, PlanExpr::BinOp(PlanBinOp::Sub, l, r)
        if **l == PlanExpr::Local(0) && **r == PlanExpr::Literal(PlanLit::Int(1)))
}

fn tot_e(mem: &BTreeSet<u32>, mul_ok: bool, calls: bool, e: &PlanExpr) -> bool {
    match e {
        PlanExpr::Literal(PlanLit::Int(_) | PlanLit::Bool(_)) | PlanExpr::Local(_) => true,
        PlanExpr::BinOp(op, l, r) => {
            (matches!(op, PlanBinOp::Add | PlanBinOp::Sub) || (*op == PlanBinOp::Mul && mul_ok))
                && tot_e(mem, mul_ok, calls, l)
                && tot_e(mem, mul_ok, calls, r)
        }
        PlanExpr::Call(PlanCallee::Fn(g), args) | PlanExpr::TailCall(g, args) => {
            calls
                && mem.contains(g)
                && args.first().is_some_and(is_descent)
                && args.iter().all(|a| tot_e(mem, mul_ok, calls, a))
        }
        _ => false,
    }
}

fn has_call(e: &PlanExpr) -> bool {
    match e {
        PlanExpr::BinOp(_, l, r) => has_call(l) || has_call(r),
        PlanExpr::Call(PlanCallee::Fn(_), _) | PlanExpr::TailCall(_, _) => true,
        _ => false,
    }
}

fn uses_mul(e: &PlanExpr) -> bool {
    match e {
        PlanExpr::BinOp(op, l, r) => *op == PlanBinOp::Mul || uses_mul(l) || uses_mul(r),
        PlanExpr::Call(_, args) | PlanExpr::TailCall(_, args) => args.iter().any(uses_mul),
        PlanExpr::If(c, t, el) => uses_mul(c) || uses_mul(t) || uses_mul(el),
        _ => false,
    }
}

fn tot_body(mem: &BTreeSet<u32>, mul_ok: bool, e: &PlanExpr) -> bool {
    match e {
        PlanExpr::If(c, base, step) => {
            matches!(c.as_ref(), PlanExpr::BinOp(PlanBinOp::Lte, l, r)
                if **l == PlanExpr::Local(0) && **r == PlanExpr::Literal(PlanLit::Int(0)))
                && tot_e(mem, mul_ok, false, base)
                && tot_e(mem, mul_ok, true, step)
                && has_call(step)
        }
        _ => false,
    }
}

/// `GrammarTotal.checkTermGroup`: `Some(uses_mul)` when the group is L3.
fn check_term_group(members: &[(u32, &FnPlan)]) -> Option<bool> {
    let mem: BTreeSet<u32> = members.iter().map(|m| m.0).collect();
    let mul = members.iter().any(|m| uses_mul(&m.1.body));
    let ok = !members.is_empty()
        && members.iter().all(|(_, p)| {
            !p.params.is_empty()
                && p.params.iter().all(|t| *t == PlanTy::Int)
                && matches!(p.ret, PlanTy::Int | PlanTy::Bool)
                && tot_body(&mem, mul, &p.body)
        });
    ok.then_some(mul)
}

// ---- report facets (`ClaimAxes.facetsE`) ----

fn facets_e(e: &PlanExpr, out: &mut BTreeSet<&'static str>) {
    match e {
        PlanExpr::Literal(PlanLit::Str(_)) => {
            out.insert("strings");
        }
        PlanExpr::Literal(PlanLit::Float(_)) => {
            out.insert("floats");
        }
        PlanExpr::Literal(_) | PlanExpr::Local(_) => {}
        PlanExpr::Let(_, v, b) => {
            facets_e(v, out);
            facets_e(b, out);
        }
        PlanExpr::Call(PlanCallee::Fn(_), args) | PlanExpr::TailCall(_, args) => {
            out.insert("calls");
            args.iter().for_each(|a| facets_e(a, out));
        }
        PlanExpr::Call(_, args) | PlanExpr::List(_, args) => {
            args.iter().for_each(|a| facets_e(a, out))
        }
        PlanExpr::BinOp(_, l, r) => {
            facets_e(l, out);
            facets_e(r, out);
        }
        PlanExpr::Neg(x) => facets_e(x, out),
        PlanExpr::If(c, t, el) => {
            facets_e(c, out);
            facets_e(t, out);
            facets_e(el, out);
        }
        PlanExpr::RecordCreate(_, fs) => {
            out.insert("records");
            fs.iter().for_each(|a| facets_e(a, out));
        }
        PlanExpr::Project(_, _, b) => {
            out.insert("records");
            facets_e(b, out);
        }
        PlanExpr::Match(s, arms) => {
            facets_e(s, out);
            for (p, b) in arms {
                match p {
                    PlanPat::Ctor(..) => {
                        out.insert("variants");
                    }
                    PlanPat::LitStr(_) => {
                        out.insert("strings");
                    }
                    PlanPat::Tuple(_) => {
                        out.insert("records");
                    }
                    _ => {}
                }
                facets_e(b, out);
            }
        }
        PlanExpr::Construct(_, _, args) => {
            out.insert("variants");
            args.iter().for_each(|a| facets_e(a, out));
        }
        PlanExpr::Interp(parts) => {
            out.insert("strings");
            parts.iter().for_each(|a| facets_e(a, out));
        }
    }
}

// ---- List helper templates (`ListHelpers.lean`) -----------------------------

/// `ListHelpers.HI`: a helper instruction, straight-line or structured.
#[derive(Clone, Debug, PartialEq)]
enum HI {
    B(BI),
    I64Add,
    Block(Vec<HI>),
    Loop(Vec<HI>),
    Br(u32),
    BrIf(u32),
    IfThen(Vec<HI>),
    IfElseI64(Vec<HI>, Vec<HI>),
    Ret,
    // The `Bytes` helpers' instructions (`BytesHelpers.lean`).
    I32Add,
    I32Sub,
    WrapI64,
    ExtU,
    I64LtU,
    Unreachable,
    IfElseI32(Vec<HI>, Vec<HI>),
    NewBytes(u64),
    GetU(u64),
    /// `local.get k; args; array.set ty`.
    SetAt(u32, u64, Vec<HI>),
    /// `local.get k; args; array.copy ty ty`.
    CopyTo(u32, u64, Vec<HI>),
}

/// `ListHelpers.HLocal`: a helper's declared local.
enum HLocal {
    I64,
    I32,
    Ref(u64),
    Val(PlanTy),
}

/// `ListHelpers.HCode`.
struct HCode {
    locals: Vec<HLocal>,
    body: Vec<HI>,
}

fn h_op(i: WI) -> HI {
    HI::B(BI::Op(i))
}

fn h_lg(i: u32) -> HI {
    h_op(WI::LocalGet(i))
}

fn h_ls(i: u32) -> HI {
    h_op(WI::LocalSet(i))
}

/// `ListHelpers.lenCode`.
fn len_code(l: u64) -> HCode {
    let body = vec![
        h_lg(1),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::I64Const(1)),
        HI::I64Add,
        h_ls(2),
        h_lg(1),
        h_op(WI::StructGet(l, 1)),
        h_ls(1),
        HI::Br(0),
    ];
    HCode {
        locals: vec![HLocal::Ref(l), HLocal::I64],
        body: vec![
            h_lg(0),
            h_ls(1),
            h_op(WI::I64Const(0)),
            h_ls(2),
            HI::Block(vec![HI::Loop(body)]),
            h_lg(2),
        ],
    }
}

/// `ListHelpers.revCode`.
fn rev_code(l: u64, t: &PlanTy) -> HCode {
    let body = vec![
        h_lg(1),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(1),
        h_op(WI::StructGet(l, 0)),
        h_ls(3),
        h_lg(3),
        h_lg(2),
        h_op(WI::StructNew(l)),
        h_ls(2),
        h_lg(1),
        h_op(WI::StructGet(l, 1)),
        h_ls(1),
        HI::Br(0),
    ];
    HCode {
        locals: vec![HLocal::Ref(l), HLocal::Ref(l), HLocal::Val(t.clone())],
        body: vec![
            h_lg(0),
            h_ls(1),
            HI::B(BI::NullOf(l)),
            h_ls(2),
            HI::Block(vec![HI::Loop(body)]),
            h_lg(2),
        ],
    }
}

/// `ListHelpers.catCode`.
fn cat_code(l: u64, r: u64) -> HCode {
    let body = vec![
        h_lg(2),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::StructGet(l, 0)),
        h_lg(3),
        h_op(WI::StructNew(l)),
        h_ls(3),
        h_lg(2),
        h_op(WI::StructGet(l, 1)),
        h_ls(2),
        HI::Br(0),
    ];
    HCode {
        locals: vec![HLocal::Ref(l), HLocal::Ref(l)],
        body: vec![
            h_lg(0),
            h_op(WI::Call(r)),
            h_ls(2),
            h_lg(1),
            h_ls(3),
            HI::Block(vec![HI::Loop(body)]),
            h_lg(3),
        ],
    }
}

/// `ListHelpers.takeCode`.
fn take_code(l: u64, r: u64) -> HCode {
    let body = vec![
        h_lg(4),
        h_lg(1),
        h_op(WI::I64GeS),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::StructGet(l, 0)),
        h_lg(3),
        h_op(WI::StructNew(l)),
        h_ls(3),
        h_lg(4),
        h_op(WI::I64Const(1)),
        HI::I64Add,
        h_ls(4),
        h_lg(2),
        h_op(WI::StructGet(l, 1)),
        h_ls(2),
        HI::Br(0),
    ];
    HCode {
        locals: vec![HLocal::Ref(l), HLocal::Ref(l), HLocal::I64],
        body: vec![
            h_lg(0),
            h_ls(2),
            HI::B(BI::NullOf(l)),
            h_ls(3),
            h_op(WI::I64Const(0)),
            h_ls(4),
            HI::Block(vec![HI::Loop(body)]),
            h_lg(3),
            h_op(WI::Call(r)),
        ],
    }
}

/// `ListHelpers.dropCode`.
fn drop_code(l: u64) -> HCode {
    let body = vec![
        h_lg(3),
        h_lg(1),
        h_op(WI::I64GeS),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::StructGet(l, 1)),
        h_ls(2),
        h_lg(3),
        h_op(WI::I64Const(1)),
        HI::I64Add,
        h_ls(3),
        HI::Br(0),
    ];
    HCode {
        locals: vec![HLocal::Ref(l), HLocal::I64],
        body: vec![
            h_lg(0),
            h_ls(2),
            h_op(WI::I64Const(0)),
            h_ls(3),
            HI::Block(vec![HI::Loop(body)]),
            h_lg(2),
        ],
    }
}

/// `ListHelpers.hasCode`.
fn has_code(l: u64, eq: WI) -> HCode {
    let body = vec![
        h_lg(2),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(2),
        h_op(WI::StructGet(l, 0)),
        h_lg(1),
        h_op(eq),
        HI::IfThen(vec![h_op(WI::I32Const(1)), HI::Ret]),
        h_lg(2),
        h_op(WI::StructGet(l, 1)),
        h_ls(2),
        HI::Br(0),
    ];
    HCode {
        locals: vec![HLocal::Ref(l)],
        body: vec![
            h_lg(0),
            h_ls(2),
            HI::Block(vec![HI::Loop(body)]),
            h_op(WI::I32Const(0)),
        ],
    }
}

/// `ListHelpers.satCode`.
fn sat_code(c: u64) -> HCode {
    HCode {
        locals: vec![],
        body: vec![
            h_lg(0),
            h_op(WI::StructGet(c, 1)),
            h_op(WI::RefIsNull),
            HI::IfElseI64(
                vec![h_lg(0), h_op(WI::StructGet(c, 0))],
                vec![
                    h_lg(0),
                    h_op(WI::StructGet(c, 2)),
                    h_op(WI::I32Const(0)),
                    h_op(WI::I32GtS),
                    HI::IfElseI64(
                        vec![h_op(WI::I64Const(i64::MAX))],
                        vec![h_op(WI::I64Const(i64::MIN))],
                    ),
                ],
            ),
        ],
    }
}

impl MCtx<'_> {
    /// `MCtx.listHelper`: the declared helper of `role` for `List<t>`.
    fn list_helper(&self, r: PlanListRole, t: &PlanTy) -> Option<u32> {
        self.tt
            .list_helpers
            .iter()
            .find(|x| &x.0 == t && x.1 == r)
            .map(|x| x.2)
    }

    /// `ListHelpers.hasEq`.
    fn has_eq(&self, t: &PlanTy) -> Option<WI> {
        match t {
            PlanTy::Int => Some(WI::Call(self.eq)),
            PlanTy::Str => Some(WI::Call(self.streq)),
            PlanTy::Bool => Some(WI::I32Eq),
            _ => None,
        }
    }

    /// `ListHelpers.helperCode`.
    fn helper_code(&self, r: PlanListRole, t: &PlanTy) -> Option<HCode> {
        let l = self.list_struct(t);
        let rev = || self.list_helper(PlanListRole::Reverse, t).map(u64::from);
        Some(match r {
            PlanListRole::Len => len_code(l),
            PlanListRole::Reverse => rev_code(l, t),
            PlanListRole::Concat => cat_code(l, rev()?),
            PlanListRole::Take => take_code(l, rev()?),
            PlanListRole::Drop => drop_code(l),
            PlanListRole::Contains => has_code(l, self.has_eq(t)?),
        })
    }

    /// The functions a helper's template calls besides its own instantiation's
    /// reverse (`ClaimAxes`: the equality contract `contains` relies on).
    fn helper_inner_calls(&self, r: PlanListRole, t: &PlanTy) -> Vec<u64> {
        match (r, t) {
            (PlanListRole::Contains, PlanTy::Int) => vec![self.eq],
            (PlanListRole::Contains, PlanTy::Str) => vec![self.streq],
            _ => vec![],
        }
    }

    /// `AcceptedArtifact.helperSig`, as decoded value types.
    fn helper_sig(&self, r: PlanListRole, t: &PlanTy) -> Option<(Vec<ValT>, Vec<ValT>)> {
        let rn = |i: u64| u32::try_from(i).ok().map(ValT::RefNull);
        let l = rn(self.list_struct(t))?;
        Some(match r {
            PlanListRole::Len => (vec![l], vec![ValT::I64]),
            PlanListRole::Reverse => (vec![l], vec![l]),
            PlanListRole::Concat => (vec![l, l], vec![l]),
            PlanListRole::Take | PlanListRole::Drop => (vec![l, ValT::I64], vec![l]),
            PlanListRole::Contains => {
                let e = match t {
                    PlanTy::Int => rn(self.carrier)?,
                    PlanTy::Str => rn(self.str_)?,
                    PlanTy::Bool => ValT::I32,
                    _ => return None,
                };
                (vec![l, e], vec![ValT::I32])
            }
        })
    }

    fn enc_h(&self, hs: &[HI], out: &mut Vec<u8>) -> Option<()> {
        for h in hs {
            match h {
                HI::B(b) => self.enc(std::slice::from_ref(b), out)?,
                HI::I64Add => out.push(0x7c),
                HI::Block(b) => {
                    out.extend([0x02, 0x40]);
                    self.enc_h(b, out)?;
                    out.push(0x0b);
                }
                HI::Loop(b) => {
                    out.extend([0x03, 0x40]);
                    self.enc_h(b, out)?;
                    out.push(0x0b);
                }
                HI::Br(d) => {
                    out.push(0x0c);
                    uleb(u64::from(*d), out)?
                }
                HI::BrIf(d) => {
                    out.push(0x0d);
                    uleb(u64::from(*d), out)?
                }
                HI::IfThen(b) => {
                    out.extend([0x04, 0x40]);
                    self.enc_h(b, out)?;
                    out.push(0x0b);
                }
                HI::IfElseI64(t, e) => {
                    out.extend([0x04, 0x7e]);
                    self.enc_h(t, out)?;
                    out.push(0x05);
                    self.enc_h(e, out)?;
                    out.push(0x0b);
                }
                HI::Ret => out.push(0x0f),
                HI::I32Add => out.push(0x6a),
                HI::I32Sub => out.push(0x6b),
                HI::WrapI64 => out.push(0xa7),
                HI::ExtU => out.push(0xad),
                HI::I64LtU => out.push(0x54),
                HI::Unreachable => out.push(0x00),
                HI::IfElseI32(t, e) => {
                    out.extend([0x04, 0x7f]);
                    self.enc_h(t, out)?;
                    out.push(0x05);
                    self.enc_h(e, out)?;
                    out.push(0x0b);
                }
                HI::NewBytes(ty) => {
                    out.extend([0xfb, 0x07]);
                    uleb(*ty, out)?
                }
                HI::GetU(ty) => {
                    out.extend([0xfb, 0x0d]);
                    uleb(*ty, out)?
                }
                HI::SetAt(k, ty, args) => {
                    out.push(0x20);
                    uleb(u64::from(*k), out)?;
                    self.enc_h(args, out)?;
                    out.extend([0xfb, 0x0e]);
                    uleb(*ty, out)?
                }
                HI::CopyTo(k, ty, args) => {
                    out.push(0x20);
                    uleb(u64::from(*k), out)?;
                    self.enc_h(args, out)?;
                    out.extend([0xfb, 0x11]);
                    uleb(*ty, out)?;
                    uleb(*ty, out)?
                }
            }
        }
        Some(())
    }

    fn h_local(&self, t: &HLocal, out: &mut Vec<u8>) -> Option<()> {
        match t {
            HLocal::I64 => out.push(0x7e),
            HLocal::I32 => out.push(0x7f),
            HLocal::Ref(ht) => {
                out.push(0x63);
                s33(*ht, out)?
            }
            HLocal::Val(t) => self.val_ty(t, out)?,
        }
        Some(())
    }

    /// `ListHelpers.hBodyBytes`, with the code entry's size prefix.
    fn h_entry_bytes(&self, c: &HCode) -> Option<Vec<u8>> {
        let mut entry = Vec::new();
        uleb(c.locals.len() as u64, &mut entry)?;
        for t in &c.locals {
            entry.push(0x01);
            self.h_local(t, &mut entry)?;
        }
        self.enc_h(&c.body, &mut entry)?;
        entry.push(0x0b);
        let mut out = Vec::new();
        uleb(entry.len() as u64, &mut out)?;
        out.extend(entry);
        Some(out)
    }

    /// The code entry the `List<t>` helper of role `r` must have.
    fn helper_entry_bytes(&self, r: PlanListRole, t: &PlanTy) -> Option<Vec<u8>> {
        self.h_entry_bytes(&self.helper_code(r, t)?)
    }

    /// The code entry `__aint_to_i64_sat` must have.
    fn sat_entry_bytes(&self) -> Option<Vec<u8>> {
        self.h_entry_bytes(&sat_code(self.carrier))
    }

    /// `GrammarLower.helperCall`.
    fn helper_call(&self, r: PlanListRole, t: &PlanTy) -> Vec<BI> {
        match self.list_helper(r, t) {
            Some(f) => vec![BI::Op(WI::Call(u64::from(f)))],
            None => vec![],
        }
    }
}

// ---- `Bytes` helper templates (`BytesHelpers.lean`) -------------------------

/// `BytesHelpers.BCode`: a helper whose locals are declared in groups.
struct BCode {
    groups: Vec<(u32, HLocal)>,
    body: Vec<HI>,
}

fn h_i32c(k: i64) -> HI {
    h_op(WI::I32Const(k))
}

/// `BytesHelpers.packCode`.
fn pack_code(l: u64, b: u64, chk: u64) -> BCode {
    let count = vec![
        h_lg(1),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        h_lg(2),
        h_i32c(1),
        HI::I32Add,
        h_ls(2),
        h_lg(1),
        h_op(WI::StructGet(l, 1)),
        h_ls(1),
        HI::Br(0),
    ];
    let fill = vec![
        h_lg(1),
        h_op(WI::RefIsNull),
        HI::BrIf(1),
        HI::SetAt(
            3,
            b,
            vec![
                h_lg(4),
                h_lg(1),
                h_op(WI::StructGet(l, 0)),
                h_op(WI::Call(chk)),
                HI::WrapI64,
            ],
        ),
        h_lg(4),
        h_i32c(1),
        HI::I32Add,
        h_ls(4),
        h_lg(1),
        h_op(WI::StructGet(l, 1)),
        h_ls(1),
        HI::Br(0),
    ];
    BCode {
        groups: vec![
            (1, HLocal::Ref(l)),
            (1, HLocal::I32),
            (1, HLocal::Ref(b)),
            (1, HLocal::I32),
        ],
        body: vec![
            h_lg(0),
            h_ls(1),
            h_i32c(0),
            h_ls(2),
            HI::Block(vec![HI::Loop(count)]),
            h_lg(2),
            HI::NewBytes(b),
            h_ls(3),
            h_lg(0),
            h_ls(1),
            h_i32c(0),
            h_ls(4),
            HI::Block(vec![HI::Loop(fill)]),
            h_lg(3),
        ],
    }
}

/// `BytesHelpers.unpackCode`.
fn unpack_code(l: u64, b: u64, box_: u64) -> BCode {
    let body = vec![
        h_lg(2),
        h_op(WI::I32Eqz),
        HI::BrIf(1),
        h_lg(2),
        h_i32c(1),
        HI::I32Sub,
        h_ls(2),
        h_lg(0),
        h_lg(2),
        HI::GetU(b),
        HI::ExtU,
        h_op(WI::Call(box_)),
        h_lg(1),
        h_op(WI::StructNew(l)),
        h_ls(1),
        HI::Br(0),
    ];
    BCode {
        groups: vec![(1, HLocal::Ref(l)), (1, HLocal::I32)],
        body: vec![
            HI::B(BI::NullOf(l)),
            h_ls(1),
            h_lg(0),
            h_op(WI::ArrayLen),
            h_ls(2),
            HI::Block(vec![HI::Loop(body)]),
            h_lg(1),
        ],
    }
}

/// `BytesHelpers.catCodeB`.
fn bytes_cat_code(b: u64) -> BCode {
    BCode {
        groups: vec![(2, HLocal::I32), (1, HLocal::Ref(b))],
        body: vec![
            h_lg(0),
            h_op(WI::ArrayLen),
            h_ls(2),
            h_lg(1),
            h_op(WI::ArrayLen),
            h_ls(3),
            h_lg(2),
            h_lg(3),
            HI::I32Add,
            HI::NewBytes(b),
            h_ls(4),
            HI::CopyTo(4, b, vec![h_i32c(0), h_lg(0), h_i32c(0), h_lg(2)]),
            HI::CopyTo(4, b, vec![h_lg(2), h_lg(1), h_i32c(0), h_lg(3)]),
            h_lg(4),
        ],
    }
}

/// `BytesHelpers.clampB`.
fn bytes_clamp() -> Vec<HI> {
    vec![
        h_lg(1),
        h_op(WI::I64Const(0)),
        h_op(WI::I64GtS),
        HI::IfThen(vec![
            h_lg(1),
            h_lg(2),
            HI::ExtU,
            HI::I64LtU,
            HI::IfElseI32(vec![h_lg(1), HI::WrapI64], vec![h_lg(2)]),
            h_ls(3),
        ]),
    ]
}

/// `BytesHelpers.takeCodeB`.
fn bytes_take_code(b: u64) -> BCode {
    let mut body = vec![h_lg(0), h_op(WI::ArrayLen), h_ls(2), h_i32c(0), h_ls(3)];
    body.extend(bytes_clamp());
    body.extend([
        h_lg(3),
        HI::NewBytes(b),
        h_ls(4),
        HI::CopyTo(4, b, vec![h_i32c(0), h_lg(0), h_i32c(0), h_lg(3)]),
        h_lg(4),
    ]);
    BCode {
        groups: vec![(2, HLocal::I32), (1, HLocal::Ref(b))],
        body,
    }
}

/// `BytesHelpers.dropCodeB`.
fn bytes_drop_code(b: u64) -> BCode {
    let mut body = vec![h_lg(0), h_op(WI::ArrayLen), h_ls(2), h_i32c(0), h_ls(3)];
    body.extend(bytes_clamp());
    body.extend([
        h_lg(2),
        h_lg(3),
        HI::I32Sub,
        h_ls(4),
        h_lg(4),
        HI::NewBytes(b),
        h_ls(5),
        HI::CopyTo(5, b, vec![h_i32c(0), h_lg(0), h_lg(3), h_lg(4)]),
        h_lg(5),
    ]);
    BCode {
        groups: vec![(3, HLocal::I32), (1, HLocal::Ref(b))],
        body,
    }
}

/// `BytesHelpers.chkCode`: `__aint_to_i64_checked`.
fn chk_code(c: u64) -> BCode {
    BCode {
        groups: vec![],
        body: vec![
            h_lg(0),
            h_op(WI::StructGet(c, 1)),
            h_op(WI::RefIsNull),
            HI::IfElseI64(
                vec![h_lg(0), h_op(WI::StructGet(c, 0))],
                vec![HI::Unreachable],
            ),
        ],
    }
}

impl MCtx<'_> {
    /// `MCtx.bytesHelper`: the declared `Bytes` helper of `role`.
    fn bytes_helper(&self, r: PlanBytesRole) -> Option<u32> {
        self.tt
            .bytes_helpers
            .iter()
            .find(|x| x.0 == r)
            .map(|x| x.1)
    }

    /// `GrammarLower.bytesCall`.
    fn bytes_call(&self, r: PlanBytesRole) -> Vec<BI> {
        match self.bytes_helper(r) {
            Some(f) => vec![BI::Op(WI::Call(u64::from(f)))],
            None => vec![],
        }
    }

    /// `BytesHelpers.bytesCode`.
    fn bytes_code(&self, r: PlanBytesRole) -> BCode {
        let l = self.list_struct(&PlanTy::Int);
        let b = self.bytes_arr;
        match r {
            PlanBytesRole::Pack => pack_code(l, b, self.to_i64_chk),
            PlanBytesRole::Unpack => unpack_code(l, b, self.box_),
            PlanBytesRole::Concat => bytes_cat_code(b),
            PlanBytesRole::Take => bytes_take_code(b),
            PlanBytesRole::Drop => bytes_drop_code(b),
        }
    }

    /// `BytesHelpers.bytesInnerCalls`: `unpack` boxes each byte.
    fn bytes_inner_calls(&self, r: PlanBytesRole) -> Vec<u64> {
        match r {
            PlanBytesRole::Unpack => vec![self.box_],
            _ => vec![],
        }
    }

    /// `AcceptedArtifact.bytesSig`, as decoded value types.
    fn bytes_sig(&self, r: PlanBytesRole) -> Option<(Vec<ValT>, Vec<ValT>)> {
        let rn = |i: u64| u32::try_from(i).ok().map(ValT::RefNull);
        let b = rn(self.bytes_arr)?;
        let l = rn(self.list_struct(&PlanTy::Int))?;
        Some(match r {
            PlanBytesRole::Pack => (vec![l], vec![b]),
            PlanBytesRole::Unpack => (vec![b], vec![l]),
            PlanBytesRole::Concat => (vec![b, b], vec![b]),
            PlanBytesRole::Take | PlanBytesRole::Drop => (vec![b, ValT::I64], vec![b]),
        })
    }

    /// `BytesHelpers.bBodyBytes`, with the code entry's size prefix.
    fn b_entry_bytes(&self, c: &BCode) -> Option<Vec<u8>> {
        let mut entry = Vec::new();
        uleb(c.groups.len() as u64, &mut entry)?;
        for (n, t) in &c.groups {
            uleb(u64::from(*n), &mut entry)?;
            self.h_local(t, &mut entry)?;
        }
        self.enc_h(&c.body, &mut entry)?;
        entry.push(0x0b);
        let mut out = Vec::new();
        uleb(entry.len() as u64, &mut out)?;
        out.extend(entry);
        Some(out)
    }

    /// The code entry the `Bytes` helper of role `r` must have.
    fn bytes_entry_bytes(&self, r: PlanBytesRole) -> Option<Vec<u8>> {
        self.b_entry_bytes(&self.bytes_code(r))
    }

    /// The code entry `__aint_to_i64_checked` must have.
    fn chk_entry_bytes(&self) -> Option<Vec<u8>> {
        self.b_entry_bytes(&chk_code(self.carrier))
    }
}
