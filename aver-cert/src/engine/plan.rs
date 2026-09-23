// ---- the one-grammar plan (schema 9) --------------------------------------
//
// A Rust mirror of `Grammar.lean`: the plan IS the optimized MIR function
// body, printed 1:1 by the compiler (`src/codegen/cert/plan_from_mir.rs`).
// This file only holds the data and its Lean rendering; nothing here decides
// anything the wall does not re-derive.

/// `Grammar.Ty`.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PlanTy {
    Int,
    Bool,
    Record(u32),
    Sum(u32),
    Option(Box<PlanTy>),
    Result(Box<PlanTy>, Box<PlanTy>),
    Eqref,
    Float,
    Str,
    Vec(Box<PlanTy>),
    List(Box<PlanTy>),
    Opaque(u32),
}

/// `Grammar.Lit`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanLit {
    Int(i64),
    Bool(bool),
    Float(u64),
    Str(Vec<u8>),
}

/// `Grammar.BinOp` (`ast::BinOp` without `Div`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlanBinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

/// `Grammar.Builtin`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlanBuiltin {
    BoolAnd,
    BoolOr,
    BoolNot,
    ListPrepend,
    VecGet,
}

/// `Grammar.LazyBuiltin`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlanLazy {
    OptWithDefault,
    ResWithDefault,
}

/// `Grammar.MirCallee`; `Fn` carries the callee's wasm function index.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlanCallee {
    Fn(u32),
    Builtin(PlanBuiltin),
    Lazy(PlanLazy),
}

/// `Grammar.CtorTag`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlanCtor {
    User(u32, u32),
    Some,
    None,
    Ok,
    Err,
}

/// The resolver's slot for an ignored binder (`Grammar.noSlot`).
pub const PLAN_NO_SLOT: u32 = 65535;

/// `Grammar.Pat`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanPat {
    Wild,
    LitInt(i64),
    LitBool(bool),
    Bind(u32),
    Ctor(PlanCtor, Vec<u32>),
    LitStr(Vec<u8>),
    Tuple(Vec<u32>),
}

/// `Grammar.Expr`; `Match` arms are `Grammar.Arms` in source order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanExpr {
    Literal(PlanLit),
    Local(u32),
    Let(u32, Box<PlanExpr>, Box<PlanExpr>),
    Call(PlanCallee, Vec<PlanExpr>),
    TailCall(u32, Vec<PlanExpr>),
    BinOp(PlanBinOp, Box<PlanExpr>, Box<PlanExpr>),
    Neg(Box<PlanExpr>),
    If(Box<PlanExpr>, Box<PlanExpr>, Box<PlanExpr>),
    RecordCreate(u32, Vec<PlanExpr>),
    Project(u32, u32, Box<PlanExpr>),
    Match(Box<PlanExpr>, Vec<(PlanPat, PlanExpr)>),
    Construct(PlanCtor, PlanTy, Vec<PlanExpr>),
    Interp(Vec<PlanExpr>),
    List(PlanTy, Vec<PlanExpr>),
}

/// `Grammar.FnPlan`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnPlan {
    pub params: Vec<PlanTy>,
    pub ret: PlanTy,
    /// Resolver slot count (parameters and every binder).
    pub nslots: u32,
    /// Declared locals past the parameters, as the emitter declares them.
    pub locals: Vec<PlanTy>,
    pub body: PlanExpr,
}

/// `Schema.RecordDecl`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PlanRecordDecl {
    pub tid: u32,
    pub struct_idx: u32,
    pub fields: Vec<PlanTy>,
}

/// `Schema.SumDecl`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PlanSumDecl {
    pub tid: u32,
    pub root: u32,
    pub ctors: Vec<(u32, Vec<PlanTy>)>,
}

/// `Schema.TypeTable`: declared layout, confirmed by the wall against the
/// type and data sections.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PlanTypeTable {
    pub carrier: Option<u32>,
    pub mag: Option<u32>,
    pub str_: Option<u32>,
    pub str_vec: Option<u32>,
    pub records: Vec<PlanRecordDecl>,
    pub sums: Vec<PlanSumDecl>,
    pub options: Vec<(PlanTy, u32)>,
    pub results: Vec<(PlanTy, PlanTy, u32)>,
    pub vecs: Vec<(PlanTy, u32)>,
    pub lists: Vec<(PlanTy, u32)>,
    pub opaques: Vec<(u32, u32)>,
    pub str_segs: Vec<(Vec<u8>, u32)>,
}

/// One user function as the compiler printed it: its wasm function index, its
/// source name (diagnostics only), and its plan or the reason it has none.
#[derive(Clone, Debug)]
pub struct PlannedFn {
    pub name: String,
    pub func_idx: u32,
    pub plan: Result<FnPlan, String>,
}

/// Everything the compiler hands the certificate producer: one entry per
/// emitted user function, the type table the plans' type ids refer to, and
/// the `__aint_eq` helper's function index (the helper is pinned by template,
/// so the index is producer data the wall confirms).
#[derive(Clone, Debug, Default)]
pub struct ModulePlans {
    pub fns: Vec<PlannedFn>,
    pub types: PlanTypeTable,
    pub aint_eq_idx: Option<u32>,
}

// ---- Lean rendering ---------------------------------------------------------

fn lean_nat_list(xs: &[u32]) -> String {
    format!(
        "[{}]",
        xs.iter().map(u32::to_string).collect::<Vec<_>>().join(", ")
    )
}

fn lean_bytes(xs: &[u8]) -> String {
    format!(
        "[{}]",
        xs.iter().map(u8::to_string).collect::<Vec<_>>().join(", ")
    )
}

fn lean_int(k: i64) -> String {
    if k < 0 {
        format!("({k})")
    } else {
        k.to_string()
    }
}

impl PlanTy {
    pub fn lean(&self) -> String {
        match self {
            PlanTy::Int => ".int".into(),
            PlanTy::Bool => ".bool".into(),
            PlanTy::Record(t) => format!("(.record {t})"),
            PlanTy::Sum(t) => format!("(.sum {t})"),
            PlanTy::Option(t) => format!("(.option {})", t.lean()),
            PlanTy::Result(t, e) => format!("(.result {} {})", t.lean(), e.lean()),
            PlanTy::Eqref => ".eqref".into(),
            PlanTy::Float => ".float".into(),
            PlanTy::Str => ".string".into(),
            PlanTy::Vec(t) => format!("(.vec {})", t.lean()),
            PlanTy::List(t) => format!("(.list {})", t.lean()),
            PlanTy::Opaque(t) => format!("(.opaque {t})"),
        }
    }
}

fn lean_ty_list(ts: &[PlanTy]) -> String {
    format!(
        "[{}]",
        ts.iter().map(PlanTy::lean).collect::<Vec<_>>().join(", ")
    )
}

impl PlanLit {
    fn lean(&self) -> String {
        match self {
            PlanLit::Int(k) => format!("(.int {})", lean_int(*k)),
            PlanLit::Bool(b) => format!("(.bool {b})"),
            PlanLit::Float(bits) => format!("(.float {bits})"),
            PlanLit::Str(bytes) => format!("(.str {})", lean_bytes(bytes)),
        }
    }
}

impl PlanBinOp {
    fn lean(self) -> &'static str {
        match self {
            PlanBinOp::Add => ".add",
            PlanBinOp::Sub => ".sub",
            PlanBinOp::Mul => ".mul",
            PlanBinOp::Eq => ".eq",
            PlanBinOp::Neq => ".neq",
            PlanBinOp::Lt => ".lt",
            PlanBinOp::Gt => ".gt",
            PlanBinOp::Lte => ".lte",
            PlanBinOp::Gte => ".gte",
        }
    }
}

impl PlanCallee {
    fn lean(self) -> String {
        match self {
            PlanCallee::Fn(f) => format!("(.fn {f})"),
            PlanCallee::Builtin(b) => format!(
                "(.builtin {})",
                match b {
                    PlanBuiltin::BoolAnd => ".boolAnd",
                    PlanBuiltin::BoolOr => ".boolOr",
                    PlanBuiltin::BoolNot => ".boolNot",
                    PlanBuiltin::ListPrepend => ".listPrepend",
                    PlanBuiltin::VecGet => ".vecGet",
                }
            ),
            PlanCallee::Lazy(l) => format!(
                "(.lazy {})",
                match l {
                    PlanLazy::OptWithDefault => ".optWithDefault",
                    PlanLazy::ResWithDefault => ".resWithDefault",
                }
            ),
        }
    }
}

impl PlanCtor {
    fn lean(self) -> String {
        match self {
            PlanCtor::User(t, c) => format!("(.user {t} {c})"),
            PlanCtor::Some => ".some".into(),
            PlanCtor::None => ".none".into(),
            PlanCtor::Ok => ".ok".into(),
            PlanCtor::Err => ".err".into(),
        }
    }
}

impl PlanPat {
    fn lean(&self) -> String {
        match self {
            PlanPat::Wild => ".wild".into(),
            PlanPat::LitInt(k) => format!("(.litInt {})", lean_int(*k)),
            PlanPat::LitBool(b) => format!("(.litBool {b})"),
            PlanPat::Bind(s) => format!("(.bind {s})"),
            PlanPat::Ctor(c, bs) => format!("(.ctor {} {})", c.lean(), lean_nat_list(bs)),
            PlanPat::LitStr(bytes) => format!("(.litStr {})", lean_bytes(bytes)),
            PlanPat::Tuple(bs) => format!("(.tuple {})", lean_nat_list(bs)),
        }
    }
}

impl PlanExpr {
    pub fn lean(&self) -> String {
        let mut out = String::new();
        self.write_lean(&mut out);
        out
    }

    fn write_list(items: &[PlanExpr], out: &mut String) {
        out.push('[');
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            item.write_lean(out);
        }
        out.push(']');
    }

    fn write_lean(&self, out: &mut String) {
        match self {
            PlanExpr::Literal(l) => {
                out.push_str("(.literal ");
                out.push_str(&l.lean());
                out.push(')');
            }
            PlanExpr::Local(i) => out.push_str(&format!("(.local {i})")),
            PlanExpr::Let(b, v, body) => {
                out.push_str(&format!("(.let_ {b} "));
                v.write_lean(out);
                out.push(' ');
                body.write_lean(out);
                out.push(')');
            }
            PlanExpr::Call(c, args) => {
                out.push_str(&format!("(.call {} ", c.lean()));
                Self::write_list(args, out);
                out.push(')');
            }
            PlanExpr::TailCall(f, args) => {
                out.push_str(&format!("(.tailCall {f} "));
                Self::write_list(args, out);
                out.push(')');
            }
            PlanExpr::BinOp(op, l, r) => {
                out.push_str(&format!("(.binOp {} ", op.lean()));
                l.write_lean(out);
                out.push(' ');
                r.write_lean(out);
                out.push(')');
            }
            PlanExpr::Neg(e) => {
                out.push_str("(.neg ");
                e.write_lean(out);
                out.push(')');
            }
            PlanExpr::If(c, t, e) => {
                out.push_str("(.ifThenElse ");
                c.write_lean(out);
                out.push(' ');
                t.write_lean(out);
                out.push(' ');
                e.write_lean(out);
                out.push(')');
            }
            PlanExpr::RecordCreate(tid, fs) => {
                out.push_str(&format!("(.recordCreate {tid} "));
                Self::write_list(fs, out);
                out.push(')');
            }
            PlanExpr::Project(tid, i, b) => {
                out.push_str(&format!("(.project {tid} {i} "));
                b.write_lean(out);
                out.push(')');
            }
            PlanExpr::Match(s, arms) => {
                out.push_str("(.match_ ");
                s.write_lean(out);
                out.push(' ');
                for (p, b) in arms {
                    out.push_str(&format!("(.cons {} ", p.lean()));
                    b.write_lean(out);
                    out.push(' ');
                }
                out.push_str(".nil");
                for _ in arms {
                    out.push(')');
                }
                out.push(')');
            }
            PlanExpr::Construct(c, ty, args) => {
                out.push_str(&format!("(.construct {} {} ", c.lean(), ty.lean()));
                Self::write_list(args, out);
                out.push(')');
            }
            PlanExpr::Interp(parts) => {
                out.push_str("(.interp ");
                Self::write_list(parts, out);
                out.push(')');
            }
            PlanExpr::List(t, items) => {
                out.push_str(&format!("(.list {} ", t.lean()));
                Self::write_list(items, out);
                out.push(')');
            }
        }
    }
}

impl FnPlan {
    /// The Lean `Grammar.FnPlan` term.
    pub fn lean(&self) -> String {
        format!(
            "{{ sig := ⟨{}, {}⟩, nslots := {}, locals := {},\n    body := {} }}",
            lean_ty_list(&self.params),
            self.ret.lean(),
            self.nslots,
            lean_ty_list(&self.locals),
            self.body.lean()
        )
    }
}

fn lean_opt_nat(v: Option<u32>) -> String {
    match v {
        Some(v) => format!("some {v}"),
        None => "none".into(),
    }
}

impl PlanTypeTable {
    /// The Lean `Schema.TypeTable` term.
    pub fn lean(&self) -> String {
        let records = self
            .records
            .iter()
            .map(|r| format!("⟨{}, {}, {}⟩", r.tid, r.struct_idx, lean_ty_list(&r.fields)))
            .collect::<Vec<_>>()
            .join(", ");
        let sums = self
            .sums
            .iter()
            .map(|s| {
                format!(
                    "⟨{}, {}, [{}]⟩",
                    s.tid,
                    s.root,
                    s.ctors
                        .iter()
                        .map(|(idx, fs)| format!("({idx}, {})", lean_ty_list(fs)))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        let pairs = |xs: &[(PlanTy, u32)]| {
            xs.iter()
                .map(|(t, i)| format!("({}, {i})", t.lean()))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let results = self
            .results
            .iter()
            .map(|(t, e, i)| format!("({}, {}, {i})", t.lean(), e.lean()))
            .collect::<Vec<_>>()
            .join(", ");
        let opaques = self
            .opaques
            .iter()
            .map(|(t, i)| format!("({t}, {i})"))
            .collect::<Vec<_>>()
            .join(", ");
        let segs = self
            .str_segs
            .iter()
            .map(|(b, i)| format!("({}, {i})", lean_bytes(b)))
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "{{ carrier := {}, mag := {}, str := {}, strVec := {},\n    records := [{records}],\n    sums := [{sums}],\n    options := [{}], results := [{results}],\n    vecs := [{}], lists := [{}], opaques := [{opaques}],\n    strSegs := [{segs}] }}",
            lean_opt_nat(self.carrier),
            lean_opt_nat(self.mag),
            lean_opt_nat(self.str_),
            lean_opt_nat(self.str_vec),
            pairs(&self.options),
            pairs(&self.vecs),
            pairs(&self.lists),
        )
    }
}
