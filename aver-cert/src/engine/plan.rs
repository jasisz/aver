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
    /// `Int.div` / `Int.mod`, admitted only fused under `Result.withDefault`
    /// with an Int literal default.
    IntDiv,
    IntMod,
}

/// `Grammar.Intrinsic`: the resolver's Euclidean discharge of `Int.div` /
/// `Int.mod` by a nonzero literal divisor.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlanIntrinsic {
    IntDivEuclid,
    IntModEuclid,
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
    Intrinsic(PlanIntrinsic),
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
    /// `[]`.
    EmptyList,
    /// `[head, ..tail]`: the head and tail slots.
    Cons(u32, u32),
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
                    PlanBuiltin::IntDiv => ".intDiv",
                    PlanBuiltin::IntMod => ".intMod",
                }
            ),
            PlanCallee::Intrinsic(i) => format!(
                "(.intrinsic {})",
                match i {
                    PlanIntrinsic::IntDivEuclid => ".intDivEuclid",
                    PlanIntrinsic::IntModEuclid => ".intModEuclid",
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
            PlanPat::EmptyList => ".emptyList".into(),
            PlanPat::Cons(h, t) => format!("(.cons {h} {t})"),
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

/// The longest list literal the type table writes in one piece. Lean
/// elaborates a list literal as one nested `List.cons` term, and a term deeper
/// than `maxRecDepth` (512 by default) fails to elaborate; btc-listener's
/// string segments (151 entries, the longest 570 bytes) went past it. A longer
/// list is written as literals of at most this many elements joined by `++`.
pub const LEAN_LIST_CHUNK: usize = 64;

/// The most rendered text one producer declaration carries in a list field.
/// Elaboration is paid per declaration (`maxHeartbeats`), so a field longer
/// than this, or with more than [`LEAN_LIST_CHUNK`] entries, is split into
/// its own declarations of at most this much text each, which the table joins
/// with `++`. The split denotes the same list: the wall reads the value, never
/// the spelling, and `decide`/`rfl` reduce the append.
pub const LEAN_TABLE_PIECE_CHARS: usize = 4096;

/// Rendered elements as one Lean list term, split by `++` into literals of at
/// most [`LEAN_LIST_CHUNK`] elements. A short list is the plain literal.
fn lean_list_chunked(items: &[String], separator: &str) -> String {
    if items.is_empty() {
        return "[]".into();
    }
    items
        .chunks(LEAN_LIST_CHUNK)
        .map(|chunk| format!("[{}]", chunk.join(separator)))
        .collect::<Vec<_>>()
        .join(" ++ ")
}

/// Split rendered list entries into pieces of at most [`LEAN_LIST_CHUNK`]
/// entries and [`LEAN_TABLE_PIECE_CHARS`] of text; an entry longer than the
/// text budget is a piece of its own.
fn table_pieces(items: &[String]) -> Vec<&[String]> {
    let mut pieces = Vec::new();
    let mut start = 0;
    let mut chars = 0;
    for (i, item) in items.iter().enumerate() {
        let full = i - start == LEAN_LIST_CHUNK || chars + item.len() > LEAN_TABLE_PIECE_CHARS;
        if i > start && full {
            pieces.push(&items[start..i]);
            start = i;
            chars = 0;
        }
        chars += item.len();
    }
    if start < items.len() {
        pieces.push(&items[start..]);
    }
    pieces
}

/// A list of rendered Lean terms of type `ty`, as the term that denotes it:
/// the plain literal when it fits one piece (see [`LEAN_TABLE_PIECE_CHARS`]),
/// else the `++` of declarations `{prefix}_{k}`, one per piece, which are
/// appended to `decls`.
pub(crate) fn lean_list_in_pieces(
    decls: &mut String,
    prefix: &str,
    ty: &str,
    items: &[String],
) -> String {
    let pieces = table_pieces(items);
    if pieces.len() <= 1 {
        return format!("[{}]", items.join(", "));
    }
    pieces
        .iter()
        .enumerate()
        .map(|(k, piece)| {
            let piece_name = format!("{prefix}_{k}");
            decls.push_str(&format!(
                "def {piece_name} : List ({ty}) :=\n  [{}]\n\n",
                piece.join(",\n   ")
            ));
            piece_name
        })
        .collect::<Vec<_>>()
        .join(" ++ ")
}

impl PlanTypeTable {
    /// The Lean declaration `def {name} : Schema.TypeTable`, preceded by the
    /// declarations `{name}_{field}_{k}` of every list field too big to write
    /// inline (see [`LEAN_TABLE_PIECE_CHARS`]); every byte list longer than
    /// [`LEAN_LIST_CHUNK`] is written in `++`-joined literals.
    pub fn lean_decls(&self, name: &str) -> String {
        self.lean_decls_and_pieces(name).0
    }

    /// The names of the piece declarations [`Self::lean_decls`] writes before
    /// `def {name}`, in order. A proof that unfolds the table by `simp` must
    /// unfold these too, or a field written in pieces stays opaque.
    pub fn lean_piece_names(&self, name: &str) -> Vec<String> {
        self.lean_decls_and_pieces(name).1
    }

    fn lean_decls_and_pieces(&self, name: &str) -> (String, Vec<String>) {
        let records = self
            .records
            .iter()
            .map(|r| format!("⟨{}, {}, {}⟩", r.tid, r.struct_idx, lean_ty_list(&r.fields)))
            .collect::<Vec<_>>();
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
            .collect::<Vec<_>>();
        let pairs = |xs: &[(PlanTy, u32)]| {
            xs.iter()
                .map(|(t, i)| format!("({}, {i})", t.lean()))
                .collect::<Vec<_>>()
        };
        let results = self
            .results
            .iter()
            .map(|(t, e, i)| format!("({}, {}, {i})", t.lean(), e.lean()))
            .collect::<Vec<_>>();
        let opaques = self
            .opaques
            .iter()
            .map(|(t, i)| format!("({t}, {i})"))
            .collect::<Vec<_>>();
        let segs = self
            .str_segs
            .iter()
            .map(|(b, i)| {
                let bytes = b.iter().map(u8::to_string).collect::<Vec<_>>();
                format!("({}, {i})", lean_list_chunked(&bytes, ", "))
            })
            .collect::<Vec<_>>();

        let mut decls = String::new();
        let mut field = |field: &str, ty: &str, items: &[String]| -> String {
            lean_list_in_pieces(&mut decls, &format!("{name}_{field}"), ty, items)
        };
        let records = field("records", "RecordDecl", &records);
        let sums = field("sums", "SumDecl", &sums);
        let options = field("options", "Ty × Nat", &pairs(&self.options));
        let results = field("results", "Ty × Ty × Nat", &results);
        let vecs = field("vecs", "Ty × Nat", &pairs(&self.vecs));
        let lists = field("lists", "Ty × Nat", &pairs(&self.lists));
        let opaques = field("opaques", "Nat × Nat", &opaques);
        let segs = field("strSegs", "List Nat × Nat", &segs);
        let pieces = decls
            .lines()
            .filter_map(|line| line.strip_prefix("def "))
            .filter_map(|rest| rest.split_once(" :").map(|(n, _)| n.to_string()))
            .collect();
        let text = format!(
            "{decls}def {name} : TypeTable :=\n  {{ carrier := {}, mag := {}, str := {}, strVec := {},\n    records := {records},\n    sums := {sums},\n    options := {options}, results := {results},\n    vecs := {vecs}, lists := {lists}, opaques := {opaques},\n    strSegs := {segs} }}\n\n",
            lean_opt_nat(self.carrier),
            lean_opt_nat(self.mag),
            lean_opt_nat(self.str_),
            lean_opt_nat(self.str_vec),
        );
        (text, pieces)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every list literal in `text`, as the count of its top-level elements.
    fn literal_lengths(text: &str) -> Vec<usize> {
        // Per open `[`: (commas at its own level, saw an element, open
        // parentheses/anonymous constructors inside it).
        let mut open: Vec<(usize, bool, usize)> = Vec::new();
        let mut lengths = Vec::new();
        for c in text.chars() {
            match c {
                '[' => {
                    if let Some(top) = open.last_mut() {
                        top.1 = true;
                    }
                    open.push((0, false, 0));
                }
                ']' => {
                    let (commas, any, _) = open.pop().expect("balanced brackets");
                    lengths.push(if any { commas + 1 } else { 0 });
                }
                '(' | '⟨' => {
                    if let Some(top) = open.last_mut() {
                        top.1 = true;
                        top.2 += 1;
                    }
                }
                ')' | '⟩' => {
                    if let Some(top) = open.last_mut() {
                        top.2 -= 1;
                    }
                }
                ',' => {
                    if let Some(top) = open.last_mut()
                        && top.2 == 0
                    {
                        top.0 += 1;
                    }
                }
                c if !c.is_whitespace() => {
                    if let Some(top) = open.last_mut() {
                        top.1 = true;
                    }
                }
                _ => {}
            }
        }
        lengths
    }

    /// The numerals of `text` in order.
    fn numerals(text: &str) -> Vec<u64> {
        text.split(|c: char| !c.is_ascii_digit())
            .filter(|s| !s.is_empty())
            .map(|s| s.parse().unwrap())
            .collect()
    }

    /// The bodies of the piece declarations of one field, in order.
    fn piece_bodies(text: &str, field: &str) -> Vec<String> {
        let prefix = format!("def types_{field}_");
        text.split("\n\n")
            .filter(|d| d.starts_with(&prefix))
            .map(|d| d.split_once(":=").unwrap().1.to_string())
            .collect()
    }

    /// Regression: btc-listener's type table (151 string segments, the
    /// longest 570 bytes) was one list literal past Lean's `maxRecDepth` and
    /// one declaration past `maxHeartbeats`. A table far bigger than that is
    /// written with no literal longer than `LEAN_LIST_CHUNK`, no piece longer
    /// than `LEAN_TABLE_PIECE_CHARS` beyond its one oversized entry, and the
    /// same value: `++` only concatenates, so every numeral appears once and
    /// in the original order.
    #[test]
    fn a_big_type_table_is_written_in_pieces_with_the_same_value() {
        let segs: Vec<(Vec<u8>, u32)> = (0..300u32)
            .map(|i| {
                let len = (i * 7 % 900) as usize;
                ((0..len).map(|b| (b % 251) as u8).collect(), i)
            })
            .collect();
        let records: Vec<PlanRecordDecl> = (0..200u32)
            .map(|tid| PlanRecordDecl {
                tid,
                struct_idx: tid + 1000,
                fields: vec![PlanTy::Int, PlanTy::Str],
            })
            .collect();
        let tt = PlanTypeTable {
            records: records.clone(),
            str_segs: segs.clone(),
            ..PlanTypeTable::default()
        };
        let text = tt.lean_decls("types");

        let longest = literal_lengths(&text).into_iter().max().unwrap_or(0);
        assert!(longest <= LEAN_LIST_CHUNK, "a literal has {longest} elements");

        let seg_pieces = piece_bodies(&text, "strSegs");
        assert!(seg_pieces.len() > 1);
        for body in &seg_pieces {
            let entries = body.matches("),\n").count() + 1;
            assert!(
                body.len() <= LEAN_TABLE_PIECE_CHARS + 16 || entries == 1,
                "a piece of {entries} entries has {} chars",
                body.len()
            );
        }
        let expected: Vec<u64> = segs
            .iter()
            .flat_map(|(bytes, idx)| bytes.iter().map(|b| *b as u64).chain([*idx as u64]))
            .collect();
        assert_eq!(numerals(&seg_pieces.concat()), expected);

        let join = text
            .lines()
            .find_map(|l| l.trim_start().strip_prefix("strSegs := "))
            .unwrap();
        let joined: Vec<&str> = join.trim_end_matches(" }").split(" ++ ").collect();
        let declared: Vec<String> = (0..seg_pieces.len())
            .map(|k| format!("types_strSegs_{k}"))
            .collect();
        assert_eq!(joined, declared);

        let expected: Vec<u64> = records
            .iter()
            .flat_map(|r| [r.tid as u64, r.struct_idx as u64])
            .collect();
        assert_eq!(numerals(&piece_bodies(&text, "records").concat()), expected);

        // The piece names are exactly the declarations written before the
        // table, so a proof unfolding the table can name every one of them.
        let names = tt.lean_piece_names("types");
        let defs: Vec<String> = text
            .lines()
            .filter_map(|l| l.strip_prefix("def "))
            .filter_map(|l| l.split_once(" :").map(|(n, _)| n.to_string()))
            .filter(|n| n != "types")
            .collect();
        assert_eq!(names, defs);
        assert!(names.iter().any(|n| n.starts_with("types_records_")));
    }

    /// A table that fits one piece is written as one declaration with one
    /// inline literal per field.
    #[test]
    fn a_small_type_table_is_one_declaration() {
        let tt = PlanTypeTable {
            carrier: Some(3),
            records: vec![PlanRecordDecl {
                tid: 0,
                struct_idx: 5,
                fields: vec![PlanTy::Int, PlanTy::Bool],
            }],
            str_segs: vec![(b"hi".to_vec(), 2)],
            ..PlanTypeTable::default()
        };
        assert_eq!(
            tt.lean_decls("types"),
            "def types : TypeTable :=\n  { carrier := some 3, mag := none, str := none, strVec := none,\n    \
             records := [⟨0, 5, [.int, .bool]⟩],\n    sums := [],\n    options := [], results := [],\n    \
             vecs := [], lists := [], opaques := [],\n    strSegs := [([104, 105], 2)] }\n\n"
        );
    }
}
