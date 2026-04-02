/// Source line number (1-based). 0 = synthetic/unknown.
pub type SourceLine = usize;

/// AST node with source location. Line-agnostic equality: two `Spanned` values
/// are equal iff their inner nodes are equal, regardless of line.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub line: SourceLine,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl<T> Spanned<T> {
    pub fn new(node: T, line: SourceLine) -> Self {
        Self { node, line }
    }

    /// Create a Spanned with line=0 (synthetic/generated AST, no source location).
    pub fn bare(node: T) -> Self {
        Self { node, line: 0 }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    pub body: Box<Spanned<Expr>>,
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
    Parsed(Box<Spanned<Expr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Attr(Box<Spanned<Expr>>, String),
    FnCall(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    BinOp(BinOp, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Match {
        subject: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },
    Constructor(String, Option<Box<Spanned<Expr>>>),
    ErrorProp(Box<Spanned<Expr>>),
    InterpolatedStr(Vec<StrPart>),
    List(Vec<Spanned<Expr>>),
    Tuple(Vec<Spanned<Expr>>),
    /// Map literal: `{"a" => 1, "b" => 2}`
    MapLiteral(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    /// Record creation: `User(name = "Alice", age = 30)`
    RecordCreate {
        type_name: String,
        fields: Vec<(String, Spanned<Expr>)>,
    },
    /// Record update: `User.update(base, field = newVal, ...)`
    RecordUpdate {
        type_name: String,
        base: Box<Spanned<Expr>>,
        updates: Vec<(String, Spanned<Expr>)>,
    },
    /// Tail-position call to a function in the same SCC (self or mutual recursion).
    /// Produced by the TCO transform pass before type-checking.
    TailCall(Box<(String, Vec<Spanned<Expr>>)>),
    /// Independent product: `(a, b, c)!` or `(a, b, c)?!`.
    /// Elements are independent effectful expressions evaluated with no guaranteed order.
    /// `unwrap=true` (`?!`): all elements must be Result; unwraps Ok values, propagates first Err.
    /// `unwrap=false` (`!`): returns raw tuple of results.
    /// Produces a replay group (effects matched by branch_path + effect_occurrence + type + args).
    IndependentProduct(Vec<Spanned<Expr>>, bool),
    /// Compiled variable lookup: `env[last][slot]` — O(1) instead of HashMap scan.
    /// Produced by the resolver pass for locals inside function bodies.
    Resolved(u16),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Binding(String, Option<String>, Spanned<Expr>),
    Expr(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody {
    Block(Vec<Stmt>),
}

impl FnBody {
    pub fn from_expr(expr: Spanned<Expr>) -> Self {
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

    pub fn tail_expr(&self) -> Option<&Spanned<Expr>> {
        match self.stmts().last() {
            Some(Stmt::Expr(expr)) => Some(expr),
            _ => None,
        }
    }

    pub fn tail_expr_mut(&mut self) -> Option<&mut Spanned<Expr>> {
        match self.stmts_mut().last_mut() {
            Some(Stmt::Expr(expr)) => Some(expr),
            _ => None,
        }
    }
}

/// Compile-time resolution metadata for a function body.
/// Produced by `resolver::resolve_fn` — maps local variable names to slot indices
/// so the interpreter can use `Vec<Value>` instead of `HashMap` lookups.
#[derive(Debug, Clone, PartialEq)]
pub struct FnResolution {
    /// Total number of local slots needed (params + bindings in body).
    pub local_count: u16,
    /// Map from local variable name → slot index in the local `Slots` frame.
    pub local_slots: std::sync::Arc<std::collections::HashMap<String, u16>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub line: usize,
    pub params: Vec<(String, String)>,
    pub return_type: String,
    pub effects: Vec<Spanned<String>>,
    pub desc: Option<String>,
    pub body: std::sync::Arc<FnBody>,
    /// `None` for unresolved (REPL, module sub-interpreters).
    pub resolution: Option<FnResolution>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub line: usize,
    pub depends: Vec<String>,
    pub exposes: Vec<String>,
    pub exposes_opaque: Vec<String>,
    pub exposes_line: Option<usize>,
    pub intent: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VerifyGivenDomain {
    /// Integer range domain in verify law: `1..50` (inclusive).
    IntRange { start: i64, end: i64 },
    /// Explicit domain values in verify law: `[v1, v2, ...]`.
    Explicit(Vec<Spanned<Expr>>),
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
    pub when: Option<Spanned<Expr>>,
    /// Template assertion from source before given-domain expansion.
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
    /// Per-sample substituted guards for `when`, aligned with `VerifyBlock.cases`.
    pub sample_guards: Vec<Spanned<Expr>>,
}

/// Source range for AST nodes that need location tracking.
/// Used by verify case spans: `cases[i] <-> case_spans[i]`.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceSpan {
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
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
    pub cases: Vec<(Spanned<Expr>, Spanned<Expr>)>,
    pub case_spans: Vec<SourceSpan>,
    /// Per-case given bindings for law verify (empty for Cases kind).
    pub case_givens: Vec<Vec<(String, Spanned<Expr>)>>,
    pub kind: VerifyKind,
}

impl VerifyBlock {
    /// Construct a VerifyBlock with default (zero) spans for each case.
    /// Use when source location tracking is not needed (codegen, tests).
    pub fn new_unspanned(
        fn_name: String,
        line: usize,
        cases: Vec<(Spanned<Expr>, Spanned<Expr>)>,
        kind: VerifyKind,
    ) -> Self {
        let case_spans = vec![SourceSpan::default(); cases.len()];
        Self {
            fn_name,
            line,
            cases,
            case_spans,
            case_givens: vec![],
            kind,
        }
    }

    pub fn iter_cases_with_spans(
        &self,
    ) -> impl Iterator<Item = (&(Spanned<Expr>, Spanned<Expr>), &SourceSpan)> {
        debug_assert_eq!(self.cases.len(), self.case_spans.len());
        self.cases.iter().zip(&self.case_spans)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecisionBlock {
    pub name: String,
    pub line: usize,
    pub date: String,
    pub reason: String,
    pub chosen: Spanned<DecisionImpact>,
    pub rejected: Vec<Spanned<DecisionImpact>>,
    pub impacts: Vec<Spanned<DecisionImpact>>,
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
}
