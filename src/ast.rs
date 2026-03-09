#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Ident(String),
    /// Empty list pattern: `[]`
    EmptyList,
    /// Cons-like list pattern: `[head, ..tail]`
    Cons(String, String),
    /// Tuple pattern: `(a, b)` / `(_, x)` / nested tuples.
    Tuple(Vec<Pattern>),
    /// Constructor pattern: fully-qualified name + list of binding names.
    /// Built-ins: Result.Ok(x), Result.Err(x), Option.Some(x), Option.None.
    /// User-defined: Shape.Circle(r), Shape.Rect(w, h), Shape.Point.
    Constructor(String, Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StrPart {
    Literal(String),
    Parsed(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Attr(Box<Expr>, String),
    FnCall(Box<Expr>, Vec<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Match {
        subject: Box<Expr>,
        arms: Vec<MatchArm>,
        line: usize,
    },
    Constructor(String, Option<Box<Expr>>),
    ErrorProp(Box<Expr>),
    InterpolatedStr(Vec<StrPart>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    /// Map literal: `{"a" => 1, "b" => 2}`
    MapLiteral(Vec<(Expr, Expr)>),
    /// Record creation: `User(name = "Alice", age = 30)`
    RecordCreate {
        type_name: String,
        fields: Vec<(String, Expr)>,
    },
    /// Record update: `User.update(base, field = newVal, ...)`
    RecordUpdate {
        type_name: String,
        base: Box<Expr>,
        updates: Vec<(String, Expr)>,
    },
    /// Tail-position call to a function in the same SCC (self or mutual recursion).
    /// Produced by the TCO transform pass before type-checking.
    /// Boxed to keep Expr enum at its original size (48 bytes).
    TailCall(Box<(String, Vec<Expr>)>),
    /// Compiled variable lookup: `env[last][slot]` — O(1) instead of HashMap scan.
    /// Produced by the resolver pass for locals inside function bodies.
    Resolved(u16),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Binding(String, Option<String>, Expr),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody {
    Block(Vec<Stmt>),
}

impl FnBody {
    pub fn from_expr(expr: Expr) -> Self {
        Self::Block(vec![Stmt::Expr(expr)])
    }

    pub fn stmts(&self) -> &[Stmt] {
        match self {
            Self::Block(stmts) => stmts,
        }
    }

    pub fn stmts_mut(&mut self) -> &mut Vec<Stmt> {
        match self {
            Self::Block(stmts) => stmts,
        }
    }

    pub fn tail_expr(&self) -> Option<&Expr> {
        match self.stmts().last() {
            Some(Stmt::Expr(expr)) => Some(expr),
            _ => None,
        }
    }

    pub fn tail_expr_mut(&mut self) -> Option<&mut Expr> {
        match self.stmts_mut().last_mut() {
            Some(Stmt::Expr(expr)) => Some(expr),
            _ => None,
        }
    }
}

/// Compile-time resolution metadata for a function body.
/// Produced by `resolver::resolve_fn` — maps local variable names to slot indices
/// so the interpreter can use `Vec<Rc<Value>>` instead of `HashMap` lookups.
#[derive(Debug, Clone, PartialEq)]
pub struct FnResolution {
    /// Total number of local slots needed (params + bindings in body).
    pub local_count: u16,
    /// Map from local variable name → slot index in the local `Slots` frame.
    pub local_slots: std::collections::HashMap<String, u16>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub line: usize,
    pub params: Vec<(String, String)>,
    pub return_type: String,
    pub effects: Vec<String>,
    pub desc: Option<String>,
    pub body: std::rc::Rc<FnBody>,
    /// `None` for unresolved (REPL, module sub-interpreters).
    pub resolution: Option<FnResolution>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub line: usize,
    pub depends: Vec<String>,
    pub exposes: Vec<String>,
    pub exposes_line: Option<usize>,
    pub intent: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VerifyGivenDomain {
    /// Integer range domain in verify law: `1..50` (inclusive).
    IntRange { start: i64, end: i64 },
    /// Explicit domain values in verify law: `[v1, v2, ...]`.
    Explicit(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifyGiven {
    pub name: String,
    pub type_name: String,
    pub domain: VerifyGivenDomain,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifyLaw {
    pub name: String,
    pub givens: Vec<VerifyGiven>,
    /// Optional precondition for the law template, written as `when <bool-expr>`.
    pub when: Option<Expr>,
    /// Template assertion from source before given-domain expansion.
    pub lhs: Expr,
    pub rhs: Expr,
    /// Per-sample substituted guards for `when`, aligned with `VerifyBlock.cases`.
    pub sample_guards: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VerifyKind {
    Cases,
    Law(Box<VerifyLaw>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifyBlock {
    pub fn_name: String,
    pub line: usize,
    pub cases: Vec<(Expr, Expr)>,
    pub kind: VerifyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecisionBlock {
    pub name: String,
    pub line: usize,
    pub date: String,
    pub reason: String,
    pub chosen: DecisionImpact,
    pub rejected: Vec<DecisionImpact>,
    pub impacts: Vec<DecisionImpact>,
    pub author: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DecisionImpact {
    Symbol(String),
    Semantic(String),
}

impl DecisionImpact {
    pub fn text(&self) -> &str {
        match self {
            DecisionImpact::Symbol(s) | DecisionImpact::Semantic(s) => s,
        }
    }

    pub fn as_context_string(&self) -> String {
        match self {
            DecisionImpact::Symbol(s) => s.clone(),
            DecisionImpact::Semantic(s) => format!("\"{}\"", s),
        }
    }
}

/// A variant in a sum type definition.
/// e.g. `Circle(Float)` → `TypeVariant { name: "Circle", fields: ["Float"] }`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariant {
    pub name: String,
    pub fields: Vec<String>, // type annotations (e.g. "Float", "String")
}

/// A user-defined type definition.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    /// `type Shape` with variants Circle(Float), Rect(Float, Float), Point
    Sum {
        name: String,
        variants: Vec<TypeVariant>,
        line: usize,
    },
    /// `record User` with fields name: String, age: Int
    Product {
        name: String,
        fields: Vec<(String, String)>,
        line: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Module(Module),
    FnDef(FnDef),
    Verify(VerifyBlock),
    Decision(DecisionBlock),
    Stmt(Stmt),
    TypeDef(TypeDef),
    /// Legacy placeholder for removed `effects X = [...]` syntax.
    EffectSet {
        name: String,
        effects: Vec<String>,
        line: usize,
    },
}
