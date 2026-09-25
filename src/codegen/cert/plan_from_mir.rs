//! The one-grammar plan printer: an optimized MIR function (exactly the tree
//! the wasm-gc MIR emitter consumed, in the same compile) printed 1:1 as a
//! `Grammar.FnPlan` (`aver_cert::FnPlan`).
//!
//! The printer is mechanical. Every admitted `MirExpr` node becomes the
//! grammar node of the same name with the same children; every other node,
//! pattern or type declines the whole function with the MIR node's name as
//! the reason. It makes no lowering choice: the wall's `GrammarLower` makes
//! those from the printed tree, and the producer checks the result against the
//! emitted code entry before anything is rendered.
//!
//! Types are printed through the emitter's own type registry (the same
//! indices the emitter wrote), into one module-wide type table whose type ids
//! the plans cite. The wall confirms every table entry against the type and
//! data sections; nothing the printer states is trusted.

use std::collections::HashMap;

use aver_cert::{
    FnPlan, ModulePlans, PLAN_NO_SLOT, PlanBinOp, PlanBuiltin, PlanCallee, PlanCtor, PlanExpr,
    PlanIntrinsic, PlanLazy, PlanLit, PlanPat, PlanRecordDecl, PlanSumDecl, PlanTy, PlanTypeTable,
    PlannedFn,
};

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::hir::{BuiltinCtor, BuiltinIntrinsic, ResolvedFnDef};
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirFn, MirPattern, MirStrPart};
use crate::ir::{BuiltinId, CtorId, FnId};

/// A user sum as the emitter laid it out: its root struct and, per
/// constructor in declaration order, the constructor's struct and field
/// types.
pub struct SumLayout {
    /// The name the emitter's registry keys the sum by: every spelling of
    /// the type (bare, module-qualified) resolves to this one.
    pub key: String,
    pub root: u32,
    pub ctors: Vec<(u32, Vec<String>)>,
}

/// A user record as the emitter laid it out: its struct (absent for a
/// one-field newtype, which the emitter erases to its field) and its declared
/// fields in order.
pub struct RecordLayout {
    /// The name the emitter's registry keys the record by: every spelling of
    /// the type (bare, module-qualified) resolves to this one, so both get
    /// one type id.
    pub key: String,
    pub struct_idx: Option<u32>,
    pub fields: Vec<(String, String)>,
}

/// The emitter's view the printer reads: the type registry, the function
/// index map and the interned builtin names of the same compile.
pub trait PlanLayout {
    fn carrier(&self) -> Option<u32>;
    fn mag(&self) -> Option<u32>;
    fn string_array(&self) -> Option<u32>;
    fn string_segment(&self, bytes: &[u8]) -> Option<u32>;
    fn option(&self, canonical: &str) -> Option<u32>;
    fn result(&self, canonical: &str) -> Option<u32>;
    fn vector(&self, canonical: &str) -> Option<u32>;
    fn list(&self, canonical: &str) -> Option<u32>;
    fn tuple(&self, canonical: &str) -> Option<u32>;
    fn map(&self, canonical: &str) -> Option<u32>;
    /// A type name the emitter represents specially (packed sequence,
    /// raw-`i64` carrier, capability resource): never printed.
    fn special(&self, name: &str) -> bool;
    fn record(&self, name: &str) -> Option<RecordLayout>;
    fn sum(&self, name: &str) -> Option<SumLayout>;
    /// A user constructor: its owning sum's name and its index in
    /// declaration order; `None` for a record's constructor.
    fn user_ctor(&self, ctor: CtorId) -> Option<(String, u32)>;
    fn fn_idx(&self, f: FnId) -> Option<u32>;
    fn builtin_name(&self, b: BuiltinId) -> Option<String>;
}

/// The reason budget of a decline.
const REASON_BUDGET: usize = 200;

fn clip(reason: String) -> String {
    if reason.len() <= REASON_BUDGET {
        return reason;
    }
    let mut end = REASON_BUDGET - 3;
    while !reason.is_char_boundary(end) {
        end -= 1;
    }
    format!("{}...", &reason[..end])
}

/// A parsed type string: `Head<args>`; a tuple is `Tuple<..>`.
#[derive(Debug, PartialEq)]
struct TyTree {
    head: String,
    args: Vec<TyTree>,
}

impl TyTree {
    fn canonical(&self) -> String {
        if self.args.is_empty() {
            self.head.clone()
        } else {
            format!(
                "{}<{}>",
                self.head,
                self.args
                    .iter()
                    .map(TyTree::canonical)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
    }
}

fn parse_ty(text: &str) -> Option<TyTree> {
    let compact: String = text.chars().filter(|c| !c.is_whitespace()).collect();
    let chars: Vec<char> = compact.chars().collect();
    let mut at = 0;
    let tree = parse_ty_at(&chars, &mut at)?;
    (at == chars.len()).then_some(tree)
}

fn parse_ty_at(chars: &[char], at: &mut usize) -> Option<TyTree> {
    if chars.get(*at) == Some(&'(') {
        *at += 1;
        let args = parse_ty_args(chars, at, ')')?;
        return Some(TyTree {
            head: "Tuple".into(),
            args,
        });
    }
    let start = *at;
    while *at < chars.len()
        && (chars[*at].is_alphanumeric() || chars[*at] == '_' || chars[*at] == '.')
    {
        *at += 1;
    }
    if start == *at {
        return None;
    }
    let head: String = chars[start..*at].iter().collect();
    if chars.get(*at) == Some(&'<') {
        *at += 1;
        let args = parse_ty_args(chars, at, '>')?;
        return Some(TyTree { head, args });
    }
    Some(TyTree {
        head,
        args: Vec::new(),
    })
}

fn parse_ty_args(chars: &[char], at: &mut usize, close: char) -> Option<Vec<TyTree>> {
    let mut args = vec![parse_ty_at(chars, at)?];
    loop {
        match chars.get(*at) {
            Some(',') => {
                *at += 1;
                args.push(parse_ty_at(chars, at)?);
            }
            Some(c) if *c == close => {
                *at += 1;
                return Some(args);
            }
            _ => return None,
        }
    }
}

/// The module-wide type table the plans cite, grown on demand.
#[derive(Default)]
pub struct TypeTableBuilder {
    table: PlanTypeTable,
    tids: HashMap<String, u32>,
    next_tid: u32,
    failed: HashMap<String, String>,
}

impl TypeTableBuilder {
    pub fn new(layout: &dyn PlanLayout) -> Self {
        Self {
            table: PlanTypeTable {
                carrier: layout.carrier(),
                mag: layout.carrier().and(layout.mag()),
                str_: layout.string_array(),
                str_vec: layout.vector("Vector<String>"),
                ..PlanTypeTable::default()
            },
            ..Self::default()
        }
    }

    pub fn finish(self) -> PlanTypeTable {
        self.table
    }

    fn tid(&mut self, key: String) -> (u32, bool) {
        if let Some(t) = self.tids.get(&key) {
            return (*t, false);
        }
        let t = self.next_tid;
        self.next_tid += 1;
        self.tids.insert(key, t);
        (t, true)
    }

    /// The heap type a representation of `ty` points at, for a newtype
    /// record's declared struct.
    fn heap_of(&self, ty: &PlanTy) -> Option<u32> {
        let t = &self.table;
        match ty {
            PlanTy::Int => t.carrier,
            PlanTy::Str => t.str_,
            PlanTy::Record(tid) => t
                .records
                .iter()
                .find(|r| r.tid == *tid)
                .map(|r| r.struct_idx),
            PlanTy::Sum(tid) => t.sums.iter().find(|s| s.tid == *tid).map(|s| s.root),
            PlanTy::Option(x) => t.options.iter().find(|o| &o.0 == x.as_ref()).map(|o| o.1),
            PlanTy::Result(x, e) => t
                .results
                .iter()
                .find(|r| &r.0 == x.as_ref() && &r.1 == e.as_ref())
                .map(|r| r.2),
            PlanTy::Vec(x) => t.vecs.iter().find(|v| &v.0 == x.as_ref()).map(|v| v.1),
            PlanTy::List(x) => t.lists.iter().find(|l| &l.0 == x.as_ref()).map(|l| l.1),
            PlanTy::Opaque(tid) => t.opaques.iter().find(|o| o.0 == *tid).map(|o| o.1),
            PlanTy::Bool | PlanTy::Float | PlanTy::Eqref => None,
        }
    }

    pub fn ty(&mut self, layout: &dyn PlanLayout, text: &str) -> Result<PlanTy, String> {
        let tree = parse_ty(text).ok_or_else(|| format!("type `{text}` does not parse"))?;
        self.ty_tree(layout, &tree)
    }

    fn ty_tree(&mut self, layout: &dyn PlanLayout, tree: &TyTree) -> Result<PlanTy, String> {
        let canon = tree.canonical();
        let arg = |this: &mut Self, i: usize| -> Result<PlanTy, String> {
            let a = tree
                .args
                .get(i)
                .ok_or_else(|| format!("type `{canon}` is missing an argument"))?;
            this.ty_tree(layout, a)
        };
        match (tree.head.as_str(), tree.args.len()) {
            ("Int", 0) => match self.table.carrier {
                Some(_) => Ok(PlanTy::Int),
                None => Err("type Int without the Int carrier".into()),
            },
            ("Bool", 0) => Ok(PlanTy::Bool),
            ("Float", 0) => Ok(PlanTy::Float),
            ("String", 0) => match self.table.str_ {
                Some(_) => Ok(PlanTy::Str),
                None => Err("type String without the string array".into()),
            },
            ("Option", 1) => {
                let t = arg(self, 0)?;
                let idx = layout
                    .option(&canon)
                    .ok_or_else(|| format!("type `{canon}` is not registered"))?;
                if !self.table.options.iter().any(|o| o.0 == t) {
                    self.table.options.push((t.clone(), idx));
                }
                Ok(PlanTy::Option(Box::new(t)))
            }
            ("Result", 2) => {
                let t = arg(self, 0)?;
                let e = arg(self, 1)?;
                let idx = layout
                    .result(&canon)
                    .ok_or_else(|| format!("type `{canon}` is not registered"))?;
                if !self.table.results.iter().any(|r| r.0 == t && r.1 == e) {
                    self.table.results.push((t.clone(), e.clone(), idx));
                }
                Ok(PlanTy::Result(Box::new(t), Box::new(e)))
            }
            ("Vector", 1) => {
                let t = arg(self, 0)?;
                let idx = layout
                    .vector(&canon)
                    .ok_or_else(|| format!("type `{canon}` is not registered"))?;
                if !self.table.vecs.iter().any(|v| v.0 == t) {
                    self.table.vecs.push((t.clone(), idx));
                }
                Ok(PlanTy::Vec(Box::new(t)))
            }
            ("List", 1) => {
                let t = arg(self, 0)?;
                let idx = layout
                    .list(&canon)
                    .ok_or_else(|| format!("type `{canon}` is not registered"))?;
                if !self.table.lists.iter().any(|l| l.0 == t) {
                    self.table.lists.push((t.clone(), idx));
                }
                Ok(PlanTy::List(Box::new(t)))
            }
            ("Tuple", n) if n >= 2 => self.record_like(
                layout,
                format!("tuple:{canon}"),
                |layout| layout.tuple(&canon),
                |this| (0..n).map(|i| arg(this, i)).collect(),
            ),
            ("Map", 2) => {
                let key = format!("opaque:{canon}");
                let idx = layout
                    .map(&canon)
                    .ok_or_else(|| format!("type `{canon}` is not registered"))?;
                let (tid, fresh) = self.tid(key);
                if fresh {
                    self.table.opaques.push((tid, idx));
                }
                Ok(PlanTy::Opaque(tid))
            }
            (name, 0) => self.named(layout, name),
            _ => Err(format!("type `{canon}`")),
        }
    }

    fn record_like(
        &mut self,
        layout: &dyn PlanLayout,
        key: String,
        struct_of: impl Fn(&dyn PlanLayout) -> Option<u32>,
        fields: impl Fn(&mut Self) -> Result<Vec<PlanTy>, String>,
    ) -> Result<PlanTy, String> {
        if let Some(reason) = self.failed.get(&key) {
            return Err(reason.clone());
        }
        if let Some(t) = self.tids.get(&key) {
            return Ok(PlanTy::Record(*t));
        }
        let idx = struct_of(layout).ok_or_else(|| format!("type `{key}` is not registered"))?;
        let (tid, _) = self.tid(key.clone());
        match fields(self) {
            Ok(fs) => {
                self.table.records.push(PlanRecordDecl {
                    tid,
                    struct_idx: idx,
                    fields: fs,
                });
                Ok(PlanTy::Record(tid))
            }
            Err(reason) => {
                self.tids.remove(&key);
                self.failed.insert(key, reason.clone());
                Err(reason)
            }
        }
    }

    fn named(&mut self, layout: &dyn PlanLayout, name: &str) -> Result<PlanTy, String> {
        if layout.special(name) {
            return Err(format!("type `{name}` has a special representation"));
        }
        if let Some(rec) = layout.record(name) {
            let key = format!("rec:{}", rec.key);
            if let Some(reason) = self.failed.get(&key) {
                return Err(reason.clone());
            }
            if let Some(t) = self.tids.get(&key) {
                return Ok(PlanTy::Record(*t));
            }
            let (tid, _) = self.tid(key.clone());
            let mut fields = Vec::new();
            for (_, fty) in &rec.fields {
                match self.ty(layout, fty) {
                    Ok(t) => fields.push(t),
                    Err(reason) => {
                        self.tids.remove(&key);
                        self.failed.insert(key, reason.clone());
                        return Err(reason);
                    }
                }
            }
            // A one-field record the emitter erases (a newtype) is represented
            // by its field's value; any other record needs two or more fields.
            let struct_idx = match (rec.struct_idx, fields.as_slice()) {
                (None, [only]) => self.heap_of(only),
                (Some(idx), fs) if fs.len() >= 2 => Some(idx),
                _ => None,
            };
            let Some(struct_idx) = struct_idx else {
                let reason = format!("record `{name}` has no struct representation");
                self.tids.remove(&key);
                self.failed.insert(key, reason.clone());
                return Err(reason);
            };
            self.table.records.push(PlanRecordDecl {
                tid,
                struct_idx,
                fields,
            });
            return Ok(PlanTy::Record(tid));
        }
        if let Some(sum) = layout.sum(name) {
            let key = format!("sum:{}", sum.key);
            if let Some(reason) = self.failed.get(&key) {
                return Err(reason.clone());
            }
            if let Some(t) = self.tids.get(&key) {
                return Ok(PlanTy::Sum(*t));
            }
            let (tid, _) = self.tid(key.clone());
            let mut ctors = Vec::new();
            for (idx, ftys) in &sum.ctors {
                let mut fields = Vec::new();
                for fty in ftys {
                    match self.ty(layout, fty) {
                        Ok(t) => fields.push(t),
                        Err(reason) => {
                            self.tids.remove(&key);
                            self.failed.insert(key, reason.clone());
                            return Err(reason);
                        }
                    }
                }
                ctors.push((*idx, fields));
            }
            self.table.sums.push(PlanSumDecl {
                tid,
                root: sum.root,
                ctors,
            });
            return Ok(PlanTy::Sum(tid));
        }
        Err(format!("type `{name}`"))
    }

    /// Declare the passive data segment holding a string literal's bytes.
    fn str_seg(&mut self, layout: &dyn PlanLayout, bytes: &[u8]) -> Result<(), String> {
        if self.table.str_segs.iter().any(|(b, _)| b == bytes) {
            return Ok(());
        }
        let seg = layout
            .string_segment(bytes)
            .ok_or("string literal without a data segment")?;
        self.table.str_segs.push((bytes.to_vec(), seg));
        Ok(())
    }

    fn sum_tid(&mut self, layout: &dyn PlanLayout, name: &str) -> Result<u32, String> {
        match self.named(layout, name)? {
            PlanTy::Sum(t) => Ok(t),
            _ => Err(format!("`{name}` is not a sum")),
        }
    }

    fn record_tid(
        &mut self,
        layout: &dyn PlanLayout,
        text: &str,
    ) -> Result<(u32, Vec<String>), String> {
        let tree = parse_ty(text).ok_or_else(|| format!("type `{text}` does not parse"))?;
        match self.ty_tree(layout, &tree)? {
            PlanTy::Record(t) => {
                let names = if tree.args.is_empty() {
                    layout
                        .record(&tree.head)
                        .map(|r| r.fields.into_iter().map(|f| f.0).collect())
                        .unwrap_or_default()
                } else {
                    Vec::new()
                };
                Ok((t, names))
            }
            _ => Err(format!("`{text}` is not a record")),
        }
    }
}

/// One function's printing context.
struct Printer<'a> {
    layout: &'a dyn PlanLayout,
    types: &'a mut TypeTableBuilder,
}

fn stamped(expr: &Spanned<MirExpr>) -> Result<String, String> {
    expr.ty()
        .map(|t| t.display())
        .ok_or_else(|| format!("{} has no type stamp", node_name(&expr.node)))
}

fn node_name(expr: &MirExpr) -> &'static str {
    match expr {
        MirExpr::Literal(_) => "Literal",
        MirExpr::Local(_) => "Local",
        MirExpr::Let(_) => "Let",
        MirExpr::Call(_) => "Call",
        MirExpr::TailCall(_) => "TailCall",
        MirExpr::BinOp(_) => "BinOp",
        MirExpr::Neg(_) => "Neg",
        MirExpr::Match(_) => "Match",
        MirExpr::Construct(_) => "Construct",
        MirExpr::RecordCreate(_) => "RecordCreate",
        MirExpr::RecordUpdate(_) => "RecordUpdate",
        MirExpr::Project(_) => "Project",
        MirExpr::IfThenElse(_) => "IfThenElse",
        MirExpr::Try(_) => "Try",
        MirExpr::List(_) => "List",
        MirExpr::Tuple(_) => "Tuple",
        MirExpr::MapLiteral(_) => "MapLiteral",
        MirExpr::InterpolatedStr(_) => "InterpolatedStr",
        MirExpr::IndependentProduct(_) => "IndependentProduct",
        MirExpr::Return(_) => "Return",
        MirExpr::FnValue(_) => "FnValue",
        MirExpr::Box(_) => "Box",
        MirExpr::Unbox(_) => "Unbox",
    }
}

fn bin_op(op: &BinOp) -> Result<PlanBinOp, String> {
    Ok(match op {
        BinOp::Add => PlanBinOp::Add,
        BinOp::Sub => PlanBinOp::Sub,
        BinOp::Mul => PlanBinOp::Mul,
        BinOp::Div => return Err("BinOp Div".into()),
        BinOp::Eq => PlanBinOp::Eq,
        BinOp::Neq => PlanBinOp::Neq,
        BinOp::Lt => PlanBinOp::Lt,
        BinOp::Gt => PlanBinOp::Gt,
        BinOp::Lte => PlanBinOp::Lte,
        BinOp::Gte => PlanBinOp::Gte,
    })
}

fn builtin_ctor(bc: &BuiltinCtor) -> PlanCtor {
    match bc {
        BuiltinCtor::OptionSome => PlanCtor::Some,
        BuiltinCtor::OptionNone => PlanCtor::None,
        BuiltinCtor::ResultOk => PlanCtor::Ok,
        BuiltinCtor::ResultErr => PlanCtor::Err,
    }
}

impl Printer<'_> {
    fn ty(&mut self, text: &str) -> Result<PlanTy, String> {
        self.types.ty(self.layout, text)
    }

    fn ctor(&mut self, ctor: &MirCtor) -> Result<PlanCtor, String> {
        match ctor {
            MirCtor::Builtin(bc) => Ok(builtin_ctor(bc)),
            MirCtor::User(id) => {
                let (parent, index) = self
                    .layout
                    .user_ctor(*id)
                    .ok_or("Construct of a record constructor")?;
                let tid = self.types.sum_tid(self.layout, &parent)?;
                Ok(PlanCtor::User(tid, index))
            }
        }
    }

    fn exprs(&mut self, items: &[Spanned<MirExpr>]) -> Result<Vec<PlanExpr>, String> {
        items.iter().map(|e| self.expr(e)).collect()
    }

    fn pat(&mut self, pattern: &MirPattern) -> Result<PlanPat, String> {
        Ok(match pattern {
            MirPattern::Wildcard => PlanPat::Wild,
            MirPattern::Literal(Literal::Int(k)) => PlanPat::LitInt(*k),
            MirPattern::Literal(Literal::Bool(b)) => PlanPat::LitBool(*b),
            MirPattern::Literal(Literal::Str(s)) => {
                self.types.str_seg(self.layout, s.as_bytes())?;
                PlanPat::LitStr(s.as_bytes().to_vec())
            }
            MirPattern::Literal(_) => {
                return Err("Match pattern Literal (non Int/Bool/String)".into());
            }
            MirPattern::Bind(slot, _) => PlanPat::Bind(slot.0),
            MirPattern::Ctor { ctor, bindings, .. } => {
                PlanPat::Ctor(self.ctor(ctor)?, bindings.iter().map(|b| b.0).collect())
            }
            MirPattern::Tuple(items) => PlanPat::Tuple(
                items
                    .iter()
                    .map(|p| match p {
                        MirPattern::Bind(slot, _) => Ok(slot.0),
                        MirPattern::Wildcard => Ok(PLAN_NO_SLOT),
                        _ => Err("Match pattern Tuple (nested)".to_string()),
                    })
                    .collect::<Result<_, _>>()?,
            ),
            MirPattern::EmptyList => return Err("Match pattern EmptyList".into()),
            MirPattern::Cons { .. } => return Err("Match pattern Cons".into()),
        })
    }

    /// `Int.div(a, b)` / `Int.mod(a, b)` as a builtin call of two arguments.
    fn int_div_builtin(&self, e: &Spanned<MirExpr>) -> Option<PlanBuiltin> {
        let MirExpr::Call(c) = &e.node else {
            return None;
        };
        let MirCallee::Builtin(b) = &c.node.callee else {
            return None;
        };
        if c.node.args.len() != 2 {
            return None;
        }
        match self.layout.builtin_name(*b)?.as_str() {
            "Int.div" => Some(PlanBuiltin::IntDiv),
            "Int.mod" => Some(PlanBuiltin::IntMod),
            _ => None,
        }
    }

    fn expr(&mut self, expr: &Spanned<MirExpr>) -> Result<PlanExpr, String> {
        Ok(match &expr.node {
            MirExpr::Literal(lit) => PlanExpr::Literal(match &lit.node {
                Literal::Int(k) => PlanLit::Int(*k),
                Literal::Bool(b) => PlanLit::Bool(*b),
                Literal::Float(f) => PlanLit::Float(f.to_bits()),
                Literal::Str(s) => {
                    self.types.str_seg(self.layout, s.as_bytes())?;
                    PlanLit::Str(s.as_bytes().to_vec())
                }
                Literal::BigInt(_) => return Err("Literal BigInt".into()),
                Literal::Unit => return Err("Literal Unit".into()),
            }),
            MirExpr::Local(local) => PlanExpr::Local(local.node.slot.0),
            MirExpr::Let(l) => {
                let l = &l.node;
                if l.binding_name.is_empty() {
                    return Err("Let (synthetic, unnamed)".into());
                }
                PlanExpr::Let(
                    l.binding.0,
                    Box::new(self.expr(&l.value)?),
                    Box::new(self.expr(&l.body)?),
                )
            }
            MirExpr::Call(c) => {
                let call = &c.node;
                match &call.callee {
                    MirCallee::Fn(f) => {
                        let idx = self
                            .layout
                            .fn_idx(*f)
                            .ok_or("Call Fn (callee has no wasm function)")?;
                        PlanExpr::Call(PlanCallee::Fn(idx), self.exprs(&call.args)?)
                    }
                    MirCallee::Builtin(b) => {
                        let name = self.layout.builtin_name(*b).unwrap_or_default();
                        // The passes keep a function unfused by this same
                        // list (`ir::cert_shape`); nothing outside it prints.
                        if !crate::ir::cert_shape::PRINTED_BUILTINS.contains(&name.as_str()) {
                            return Err(format!("Call Builtin({name})"));
                        }
                        let callee = match name.as_str() {
                            "Bool.and" => PlanCallee::Builtin(PlanBuiltin::BoolAnd),
                            "Bool.or" => PlanCallee::Builtin(PlanBuiltin::BoolOr),
                            "Bool.not" => PlanCallee::Builtin(PlanBuiltin::BoolNot),
                            "List.prepend" => PlanCallee::Builtin(PlanBuiltin::ListPrepend),
                            "Vector.get" => PlanCallee::Builtin(PlanBuiltin::VecGet),
                            "Option.withDefault" => PlanCallee::Lazy(PlanLazy::OptWithDefault),
                            "Result.withDefault" => {
                                // `Int.div` / `Int.mod` print only fused here.
                                if let [o, d] = call.args.as_slice()
                                    && let Some(op) = self.int_div_builtin(o)
                                {
                                    let MirExpr::Call(inner) = &o.node else {
                                        unreachable!("int_div_builtin matched a call")
                                    };
                                    if !matches!(&d.node, MirExpr::Literal(l)
                                        if matches!(l.node, Literal::Int(_)))
                                    {
                                        return Err(format!(
                                            "Call Builtin(Result.withDefault) over {} \
                                             with a default that is not an Int literal",
                                            if op == PlanBuiltin::IntDiv {
                                                "Int.div"
                                            } else {
                                                "Int.mod"
                                            }
                                        ));
                                    }
                                    let inner_args = self.exprs(&inner.node.args)?;
                                    return Ok(PlanExpr::Call(
                                        PlanCallee::Lazy(PlanLazy::ResWithDefault),
                                        vec![
                                            PlanExpr::Call(PlanCallee::Builtin(op), inner_args),
                                            self.expr(d)?,
                                        ],
                                    ));
                                }
                                PlanCallee::Lazy(PlanLazy::ResWithDefault)
                            }
                            other @ ("Int.div" | "Int.mod") => {
                                return Err(format!(
                                    "Call Builtin({other}) outside Result.withDefault (a Result<Int, String>)"
                                ));
                            }
                            other => return Err(format!("Call Builtin({other})")),
                        };
                        PlanExpr::Call(callee, self.exprs(&call.args)?)
                    }
                    MirCallee::Intrinsic(i) => {
                        let intrinsic = match i {
                            BuiltinIntrinsic::IntDivEuclid => PlanIntrinsic::IntDivEuclid,
                            BuiltinIntrinsic::IntModEuclid => PlanIntrinsic::IntModEuclid,
                            other => return Err(format!("Call Intrinsic({other:?})")),
                        };
                        PlanExpr::Call(PlanCallee::Intrinsic(intrinsic), self.exprs(&call.args)?)
                    }
                    MirCallee::LocalSlot { .. } => return Err("Call LocalSlot".into()),
                }
            }
            MirExpr::TailCall(tc) => {
                let idx = self
                    .layout
                    .fn_idx(tc.node.target)
                    .ok_or("TailCall (target has no wasm function)")?;
                PlanExpr::TailCall(idx, self.exprs(&tc.node.args)?)
            }
            MirExpr::BinOp(b) => PlanExpr::BinOp(
                bin_op(&b.node.op)?,
                Box::new(self.expr(&b.node.lhs)?),
                Box::new(self.expr(&b.node.rhs)?),
            ),
            MirExpr::Neg(_) => return Err("Neg (no Int negation helper template)".into()),
            MirExpr::IfThenElse(ite) => PlanExpr::If(
                Box::new(self.expr(&ite.node.cond)?),
                Box::new(self.expr(&ite.node.then_branch)?),
                Box::new(self.expr(&ite.node.else_branch)?),
            ),
            MirExpr::RecordCreate(r) => {
                let rec = &r.node;
                let (tid, declared) = self.types.record_tid(self.layout, &rec.type_name)?;
                if declared.len() < 2 {
                    return Err("RecordCreate of a one-field record (newtype)".into());
                }
                let written: Vec<&str> = rec.fields.iter().map(|f| f.name.as_str()).collect();
                if written != declared.iter().map(String::as_str).collect::<Vec<_>>() {
                    return Err("RecordCreate (fields not in declared order)".into());
                }
                let values: Vec<Spanned<MirExpr>> =
                    rec.fields.iter().map(|f| f.value.clone()).collect();
                PlanExpr::RecordCreate(tid, self.exprs(&values)?)
            }
            MirExpr::Project(p) => {
                let base_ty = stamped(&p.node.base)?;
                let (tid, declared) = self.types.record_tid(self.layout, &base_ty)?;
                if declared.len() < 2 {
                    return Err("Project of a one-field record (newtype)".into());
                }
                let field = declared
                    .iter()
                    .position(|f| f == &p.node.field)
                    .ok_or("Project (field not declared)")?;
                PlanExpr::Project(tid, field as u32, Box::new(self.expr(&p.node.base)?))
            }
            MirExpr::Match(m) => {
                let subject = self.expr(&m.node.subject)?;
                let mut arms = Vec::with_capacity(m.node.arms.len());
                for arm in &m.node.arms {
                    arms.push((self.pat(&arm.pattern)?, self.expr(&arm.body)?));
                }
                PlanExpr::Match(Box::new(subject), arms)
            }
            MirExpr::Construct(c) => {
                let ctor = self.ctor(&c.node.ctor)?;
                let ty = self.ty(&stamped(expr)?)?;
                PlanExpr::Construct(ctor, ty, self.exprs(&c.node.args)?)
            }
            MirExpr::InterpolatedStr(parts) => PlanExpr::Interp(
                parts
                    .iter()
                    .map(|part| match part {
                        MirStrPart::Literal(s) => {
                            self.types.str_seg(self.layout, s.as_bytes())?;
                            Ok(PlanExpr::Literal(PlanLit::Str(s.as_bytes().to_vec())))
                        }
                        MirStrPart::Expr(e) => {
                            if stamped(e)? != "String" {
                                return Err("InterpolatedStr (a part is not a String)".into());
                            }
                            self.expr(e)
                        }
                    })
                    .collect::<Result<_, _>>()?,
            ),
            MirExpr::List(items) => {
                if !items.is_empty() {
                    return Err("List (non-empty literal)".into());
                }
                let ty = self.ty(&stamped(expr)?)?;
                let PlanTy::List(elem) = ty else {
                    return Err("List (stamp is not a List)".into());
                };
                PlanExpr::List(*elem, Vec::new())
            }
            other => return Err(node_name(other).to_string()),
        })
    }
}

/// Print one function. `extra_locals` is the emitter's declared locals vector
/// past the parameters, as the code entry declares it.
pub fn print_fn(
    mir_fn: &MirFn,
    rfd: &ResolvedFnDef,
    extra_locals: &[wasm_encoder::ValType],
    layout: &dyn PlanLayout,
    types: &mut TypeTableBuilder,
) -> Result<FnPlan, String> {
    print_fn_inner(mir_fn, rfd, extra_locals, layout, types).map_err(clip)
}

fn print_fn_inner(
    mir_fn: &MirFn,
    rfd: &ResolvedFnDef,
    extra_locals: &[wasm_encoder::ValType],
    layout: &dyn PlanLayout,
    types: &mut TypeTableBuilder,
) -> Result<FnPlan, String> {
    if !mir_fn.effects.is_empty() {
        return Err("fn declares effects".into());
    }
    let repr = &mir_fn.repr;
    if repr.bare_return
        || !repr.bare_slots.is_empty()
        || !repr.carrier_slots.is_empty()
        || repr.bare_params.iter().any(|b| *b)
    {
        return Err("raw i64 slots (D5)".into());
    }
    for (i, param) in mir_fn.params.iter().enumerate() {
        if param.local.0 as usize != i {
            return Err("parameter slots are not 0..n".into());
        }
    }
    let resolution = rfd.resolution.as_ref().ok_or("fn has no slot resolution")?;
    let slot_types = resolution.local_slot_types.as_ref();
    let nparams = rfd.params.len();
    if nparams != mir_fn.params.len() || slot_types.len() < nparams {
        return Err("parameter count mismatch".into());
    }
    let mut printer = Printer { layout, types };
    let params = rfd
        .params
        .iter()
        .map(|(_, t)| printer.ty(&t.display()))
        .collect::<Result<Vec<_>, _>>()?;
    let ret = printer.ty(&rfd.return_type.display())?;
    let nslots = slot_types.len();
    let mut locals = Vec::with_capacity(extra_locals.len());
    for (j, vt) in extra_locals.iter().enumerate() {
        let slot = nparams + j;
        let ty = if slot < nslots {
            let t = &slot_types[slot];
            let text = t.display();
            match text.as_str() {
                // An `i32` placeholder the emitter keeps for index alignment.
                "Unit" | "Invalid" => PlanTy::Bool,
                _ => printer.ty(&text)?,
            }
        } else {
            scratch_ty(vt, printer.types).ok_or("scratch local of an unprintable type")?
        };
        locals.push(ty);
    }
    let body = printer.expr(&mir_fn.body)?;
    Ok(FnPlan {
        params,
        ret,
        nslots: nslots as u32,
        locals,
        body,
    })
}

/// A scratch local (past the resolver slots) by its declared wasm type: the
/// subject scratch `eqref`, an Int carrier scratch, or an `i32`.
fn scratch_ty(vt: &wasm_encoder::ValType, types: &TypeTableBuilder) -> Option<PlanTy> {
    use wasm_encoder::{AbstractHeapType, HeapType, ValType};
    match vt {
        ValType::I32 => Some(PlanTy::Bool),
        ValType::F64 => Some(PlanTy::Float),
        ValType::Ref(rt) if rt.nullable => match rt.heap_type {
            HeapType::Abstract {
                ty: AbstractHeapType::Eq,
                shared: false,
            } => Some(PlanTy::Eqref),
            HeapType::Concrete(idx) if Some(idx) == types.table.carrier => Some(PlanTy::Int),
            HeapType::Concrete(idx) if Some(idx) == types.table.str_ => Some(PlanTy::Str),
            _ => None,
        },
        _ => None,
    }
}

/// One emitted user function as `print_module` reads it: its wasm function
/// index, its MIR (when it lowered), its resolved definition and its declared
/// extra locals.
pub type EmittedFn<'a> = (
    u32,
    Option<&'a MirFn>,
    &'a ResolvedFnDef,
    Option<&'a [wasm_encoder::ValType]>,
);

/// Print every emitted user function, one `EmittedFn` per user function.
pub fn print_module(
    fns: &[EmittedFn<'_>],
    layout: &dyn PlanLayout,
    aint_eq_idx: Option<u32>,
) -> ModulePlans {
    let mut types = TypeTableBuilder::new(layout);
    let mut out = Vec::with_capacity(fns.len());
    for (func_idx, mir_fn, rfd, extra) in fns {
        let plan = match (mir_fn, extra) {
            (Some(mir_fn), Some(extra)) => print_fn(mir_fn, rfd, extra, layout, &mut types),
            _ => Err("fn has no MIR body (trap stub)".to_string()),
        };
        out.push(PlannedFn {
            name: rfd.name.clone(),
            func_idx: *func_idx,
            plan,
        });
    }
    ModulePlans {
        fns: out,
        types: types.finish(),
        aint_eq_idx,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_strings_parse_into_trees() {
        let t = parse_ty("Result<Option<Int>, (Int, String)>").expect("parses");
        assert_eq!(t.canonical(), "Result<Option<Int>,Tuple<Int,String>>");
        assert_eq!(
            parse_ty("Map<String,Json.Json>").unwrap().canonical(),
            "Map<String,Json.Json>"
        );
        assert!(parse_ty("Option<Int").is_none());
        assert!(parse_ty("").is_none());
    }

    #[test]
    fn decline_reasons_stay_within_the_budget() {
        let long = "x".repeat(500);
        let clipped = clip(long);
        assert!(clipped.len() <= REASON_BUDGET);
        assert!(clipped.ends_with("..."));
        assert_eq!(clip("Neg".into()), "Neg");
    }

    /// Compile `src` through the real pipeline and return every user
    /// function's printed plan (or decline reason) by name.
    fn plans(src: &str) -> (HashMap<String, Result<FnPlan, String>>, PlanTypeTable) {
        let mut items = crate::source::parse_source(src).expect("parse");
        use crate::ir::{PipelineConfig, TypecheckMode};
        let result = crate::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                run_interp_lower: false,
                run_buffer_build: false,
                ..Default::default()
            },
        );
        if let Some(tc) = &result.typecheck {
            assert!(tc.errors.is_empty(), "typecheck: {:?}", tc.errors);
        }
        let out = crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(
            &items, None, None,
        )
        .expect("compile");
        let map = out
            .cert_plans
            .fns
            .into_iter()
            .map(|f| (f.name, f.plan))
            .collect();
        (map, out.cert_plans.types)
    }

    fn plan(map: &HashMap<String, Result<FnPlan, String>>, name: &str) -> FnPlan {
        map.get(name)
            .unwrap_or_else(|| panic!("no fn {name}"))
            .clone()
            .unwrap_or_else(|reason| panic!("{name} declined: {reason}"))
    }

    fn reason(map: &HashMap<String, Result<FnPlan, String>>, name: &str) -> String {
        match map.get(name).unwrap_or_else(|| panic!("no fn {name}")) {
            Ok(_) => panic!("{name} was printed"),
            Err(reason) => reason.clone(),
        }
    }

    const SRC: &str = r#"
module P
    intent = "printer probes"
    exposes [lit, add, cmpLit, andOr, named, pick, callee, caller, tail, mk, get, isNone, opt, shape, circle, area, greet, strMatch, intMatch, empty, pair, neg, divide, effectful, lst]
    effects [Console]

record Point
    x: Int
    y: Int

type Shape
    Circle(Int)
    Square(Int)

fn lit() -> Int
    7

fn add(a: Int, b: Int) -> Int
    a + b

fn cmpLit(a: Int) -> Bool
    a > 5

fn andOr(p: Bool, q: Bool) -> Bool
    Bool.or(Bool.and(p, q), Bool.not(p))

fn named(n: Int) -> Int
    m = n + 2
    m * m

fn pick(b: Bool, x: Int, y: Int) -> Int
    match b
        true -> x
        false -> y

fn callee(x: Int) -> Int
    x + 1

fn caller(x: Int) -> Int
    callee(callee(x))

fn tail(n: Int, acc: Int) -> Int
    match n <= 0
        true -> acc
        false -> tail(n - 1, acc + n)

fn mk(a: Int, b: Int) -> Point
    Point(x = a, y = b)

fn get(p: Point) -> Int
    p.y

fn isNone(o: Option<Int>) -> Bool
    match o
        Option.Some(_) -> false
        Option.None -> true

fn opt(o: Option<Int>) -> Int
    Option.withDefault(o, 0)

fn circle(r: Int) -> Shape
    Shape.Circle(r)

fn area(s: Shape) -> Int
    match s
        Shape.Circle(r) -> r * r
        Shape.Square(w) -> w * w

fn shape(s: Shape) -> Bool
    match s
        Shape.Circle(_) -> true
        _ -> false

fn greet(name: String) -> String
    "Hello, {name}"

fn strMatch(s: String) -> String
    match s
        "ab" -> "b"
        _ -> s

fn intMatch(n: Int) -> Int
    match n
        0 -> 10
        1 -> 11
        _ -> 12

fn empty() -> List<Int>
    []

fn pair(t: Tuple<Int, Int>) -> Int
    match t
        (a, _) -> a

fn neg(x: Int) -> Int
    -x

fn divide(a: Float, b: Float) -> Float
    a / b

fn effectful() -> Unit
    ! [Console.print]
    Console.print("x")

fn lst(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [h, ..t] -> h
"#;

    #[test]
    fn every_admitted_node_prints_one_to_one() {
        let (map, types) = plans(SRC);
        use PlanExpr as E;
        let l = E::Local;
        let int = |k: i64| E::Literal(PlanLit::Int(k));
        let bin = |op, a, b| E::BinOp(op, Box::new(a), Box::new(b));
        assert_eq!(plan(&map, "lit").body, int(7));
        let add = plan(&map, "add");
        assert_eq!(add.params, [PlanTy::Int, PlanTy::Int]);
        assert_eq!(add.body, bin(PlanBinOp::Add, l(0), l(1)));
        assert_eq!(plan(&map, "cmpLit").body, bin(PlanBinOp::Gt, l(0), int(5)));
        assert!(matches!(
            plan(&map, "andOr").body,
            E::Call(PlanCallee::Builtin(PlanBuiltin::BoolOr), _)
        ));
        assert!(matches!(plan(&map, "named").body, E::Let(1, _, _)));
        assert!(matches!(plan(&map, "pick").body, E::If(..) | E::Match(..)));
        let caller = plan(&map, "caller");
        let E::Call(PlanCallee::Fn(callee_idx), args) = &caller.body else {
            panic!("caller body: {:?}", caller.body)
        };
        assert!(matches!(&args[0], E::Call(PlanCallee::Fn(i), _) if i == callee_idx));
        assert!(format!("{:?}", plan(&map, "tail").body).contains("TailCall"));
        assert!(matches!(plan(&map, "mk").body, E::RecordCreate(_, ref fs) if fs.len() == 2));
        assert!(matches!(plan(&map, "get").body, E::Project(_, 1, _)));
        assert!(matches!(
            plan(&map, "isNone").body,
            E::Match(_, ref arms) if matches!(arms[0].0, PlanPat::Ctor(PlanCtor::Some, _))
        ));
        assert!(matches!(
            plan(&map, "opt").body,
            E::Call(PlanCallee::Lazy(PlanLazy::OptWithDefault), _)
        ));
        assert!(matches!(
            plan(&map, "circle").body,
            E::Construct(PlanCtor::User(_, 0), PlanTy::Sum(_), _)
        ));
        let area = plan(&map, "area");
        assert!(matches!(area.body, E::Match(_, ref arms) if arms.len() == 2));
        assert!(
            area.locals.contains(&PlanTy::Eqref),
            "subject scratch declared"
        );
        assert!(matches!(
            plan(&map, "greet").body,
            E::Interp(ref parts) if parts.len() == 2
        ));
        assert!(matches!(
            plan(&map, "strMatch").body,
            E::Match(_, ref arms) if matches!(arms[0].0, PlanPat::LitStr(_))
        ));
        assert!(matches!(
            plan(&map, "intMatch").body,
            E::Match(_, ref arms) if arms[0].0 == PlanPat::LitInt(0) && arms[2].0 == PlanPat::Wild
        ));
        assert_eq!(plan(&map, "empty").body, E::List(PlanTy::Int, vec![]));
        assert!(matches!(
            plan(&map, "pair").body,
            E::Match(_, ref arms) if matches!(arms[0].0, PlanPat::Tuple(ref bs) if bs[1] == PLAN_NO_SLOT)
        ));
        // The type table names what the plans cite, from the emitter's registry.
        assert!(types.carrier.is_some() && types.str_.is_some());
        assert!(
            types
                .records
                .iter()
                .any(|r| r.fields == [PlanTy::Int, PlanTy::Int])
        );
        assert!(types.sums.iter().any(|s| s.ctors.len() == 2));
        assert!(types.str_segs.iter().any(|(b, _)| b == b"Hello, "));
    }

    const DIV_SRC: &str = r#"
module D
    intent = "division printer probes"
    exposes [halve, low, guarded, guardedMod, unfused, notLiteral]

fn halve(p: Int) -> Int
    Int.div(p, 2)

fn low(v: Int) -> Int
    Int.mod(v, 256)

fn guarded(a: Int, d: Int) -> Int
    Result.withDefault(Int.div(a, d), 0)

fn guardedMod(a: Int, d: Int) -> Int
    Result.withDefault(Int.mod(a, d), 7)

fn unfused(a: Int, d: Int) -> Result<Int, String>
    Int.div(a, d)

fn notLiteral(a: Int, d: Int) -> Int
    Result.withDefault(Int.div(a, d), a)
"#;

    /// `Int.div` / `Int.mod` print as the resolver left them: the literal
    /// divisor as the Euclidean intrinsic, a variable divisor only fused
    /// under `Result.withDefault` with an Int literal default.
    #[test]
    fn division_prints_as_intrinsic_or_fused_default() {
        let (map, _) = plans(DIV_SRC);
        use PlanExpr as E;
        let l = E::Local;
        let int = |k: i64| E::Literal(PlanLit::Int(k));
        assert_eq!(
            plan(&map, "halve").body,
            E::Call(
                PlanCallee::Intrinsic(PlanIntrinsic::IntDivEuclid),
                vec![l(0), int(2)]
            )
        );
        assert_eq!(
            plan(&map, "low").body,
            E::Call(
                PlanCallee::Intrinsic(PlanIntrinsic::IntModEuclid),
                vec![l(0), int(256)]
            )
        );
        let fused = |op, d| {
            E::Call(
                PlanCallee::Lazy(PlanLazy::ResWithDefault),
                vec![E::Call(PlanCallee::Builtin(op), vec![l(0), l(1)]), int(d)],
            )
        };
        assert_eq!(plan(&map, "guarded").body, fused(PlanBuiltin::IntDiv, 0));
        assert_eq!(plan(&map, "guardedMod").body, fused(PlanBuiltin::IntMod, 7));
        assert!(reason(&map, "unfused").contains("outside Result.withDefault"));
        assert!(reason(&map, "notLiteral").contains("not an Int literal"));
    }

    #[test]
    fn unadmitted_nodes_decline_by_name() {
        let (map, _) = plans(SRC);
        assert_eq!(reason(&map, "neg"), "Neg (no Int negation helper template)");
        assert_eq!(reason(&map, "divide"), "BinOp Div");
        assert_eq!(reason(&map, "effectful"), "fn declares effects");
        assert_eq!(reason(&map, "lst"), "Match pattern EmptyList");
    }
}
