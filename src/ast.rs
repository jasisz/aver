#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
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
    /// Constructor pattern: name + list of binding names.
    /// Built-ins: Ok(x), Err(x), Some(x), None → vec!["x"] or vec![]
    /// User-defined: Circle(r), Rect(w, h), Point → vec!["r"], vec!["w","h"], vec![]
    Constructor(String, Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StrPart {
    Literal(String),
    Expr(String), // raw expression source text
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Attr(Box<Expr>, String),
    FnCall(Box<Expr>, Vec<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<MatchArm>),
    Pipe(Box<Expr>, Box<Expr>),
    Constructor(String, Option<Box<Expr>>),
    ErrorProp(Box<Expr>),
    InterpolatedStr(Vec<StrPart>),
    List(Vec<Expr>),
    /// Explicit expression type annotation: `expr: Type`
    TypeAscription(Box<Expr>, String),
    /// Record creation: `User(name: "Alice", age: 30)`
    RecordCreate {
        type_name: String,
        fields: Vec<(String, Expr)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Val(String, Expr),
    Var(String, Expr, Option<String>),
    /// Bare assignment to an existing `var` binding: `name = expr`
    Assign(String, Expr),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody {
    Expr(Expr),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub return_type: String,
    pub effects: Vec<String>,
    pub desc: Option<String>,
    pub body: FnBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub depends: Vec<String>,
    pub exposes: Vec<String>,
    pub intent: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerifyBlock {
    pub fn_name: String,
    pub cases: Vec<(Expr, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecisionBlock {
    pub name: String,
    pub date: String,
    pub reason: String,
    pub chosen: String,
    pub rejected: Vec<String>,
    pub impacts: Vec<String>,
    pub author: Option<String>,
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
    },
    /// `record User` with fields name: String, age: Int
    Product {
        name: String,
        fields: Vec<(String, String)>,
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
    /// `effects AppIO = [Console, Disk]` — named effect set (alias)
    EffectSet {
        name: String,
        effects: Vec<String>,
    },
}
