pub mod types;
pub mod unparse;
pub use types::Type;

/// Source line number (1-based). 0 = synthetic/unknown.
pub type SourceLine = usize;

/// A `bool` that compares as always-equal. Used for `last_use` annotations
/// on `Expr::Resolved` — metadata that should not affect AST equality
/// (same pattern as `Spanned` ignoring `line` in its `PartialEq`).
#[derive(Debug, Clone, Copy, Default)]
pub struct AnnotBool(pub bool);

impl PartialEq for AnnotBool {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl From<bool> for AnnotBool {
    fn from(b: bool) -> Self {
        Self(b)
    }
}

/// AST node with source location plus an optional inferred type.
///
/// Line-agnostic equality: two `Spanned` values are equal iff their inner
/// nodes are equal, regardless of line or attached type. The type slot is a
/// `OnceLock<Type>` populated by the type checker; backends that have not
/// been migrated to consume it stay agnostic and continue inferring locally.
/// `OnceLock` (rather than `OnceCell`) keeps `Spanned` `Sync`, which matters
/// because parts of the AST live behind `Arc` and cross thread boundaries
/// (e.g. parallel verify execution, REPL background tasks).
#[derive(Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub line: SourceLine,
    pub ty: std::sync::OnceLock<Type>,
}

// `OnceLock` does not derive `Clone` (the cell is invariant over `T`), so the
// inner type is cloned manually.
impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        let ty = std::sync::OnceLock::new();
        if let Some(t) = self.ty.get() {
            let _ = ty.set(t.clone());
        }
        Self {
            node: self.node.clone(),
            line: self.line,
            ty,
        }
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl<T> Spanned<T> {
    pub fn new(node: T, line: SourceLine) -> Self {
        Self {
            node,
            line,
            ty: std::sync::OnceLock::new(),
        }
    }

    /// Create a Spanned with line=0 (synthetic/generated AST, no source location).
    pub fn bare(node: T) -> Self {
        Self::new(node, 0)
    }

    /// Record the inferred type for this node. No-op if a type is already set
    /// (later inference passes must not contradict the first one).
    pub fn set_ty(&self, ty: Type) {
        let _ = self.ty.set(ty);
    }

    /// Inferred type for this node, if the type checker has visited it.
    pub fn ty(&self) -> Option<&Type> {
        self.ty.get()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    /// An integer literal whose magnitude does not fit `i64`. Aver's `Int` is
    /// arbitrary-precision (ℤ) at runtime, so such a literal is kept as its
    /// decimal digit string (unsigned magnitude — any sign is the surrounding
    /// negation/subtraction, exactly as for `Int`) and lowered to the same
    /// arbitrary-precision construction `Int.n("…")` uses on every backend.
    /// Small literals stay `Int(i64)` so the common path is byte-identical.
    BigInt(String),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
}

/// Literal-divisor discharge predicate, shared by the typechecker and the
/// HIR resolver: `Int.div(a, K)` / `Int.mod(a, K)` type and lower as TOTAL
/// (plain `Int`, direct Euclidean division) exactly when the divisor `K` is
/// a syntactic nonzero integer literal — `2`, `-3` (a single unary minus
/// over a literal), or a `BigInt` literal (whose magnitude exceeds `i64`,
/// so it is nonzero by construction). Everything else — a `0` literal, a
/// double negation (`--5`), an identifier, a named constant, a constant
/// expression like `8 + 8` — keeps the `Result<Int, String>` path. The
/// boundary is deliberately syntactic: this is a typing rule for two
/// builtin callees, not a refinement system, and the typechecker stays
/// solver-free.
///
/// Both consumers MUST share this one predicate. Real pipelines resolve
/// without typechecking (`tests/eval_spec.rs` compiles straight from the
/// resolver), so the HIR rewrite cannot key on type stamps — a stamp-keyed
/// rewrite would fork semantics between checked and unchecked pipelines.
pub fn is_literal_nonzero_int_divisor(expr: &Spanned<Expr>) -> bool {
    fn is_nonzero_int_literal(node: &Expr) -> bool {
        match node {
            Expr::Literal(Literal::Int(k)) => *k != 0,
            // The lexer produces `BigInt` only when the magnitude overflows
            // `i64`, so a BigInt literal is never zero.
            Expr::Literal(Literal::BigInt(_)) => true,
            _ => false,
        }
    }
    match &expr.node {
        Expr::Neg(inner) => is_nonzero_int_literal(&inner.node),
        node => is_nonzero_int_literal(node),
    }
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

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Spanned<Expr>>,
    /// Per-arm slot table for the pattern's bindings, in pattern order.
    /// Filled by the resolver pass; backend code reads from here
    /// instead of doing a name lookup, so two arms with the same
    /// binding name (e.g. `deadline` showing up in both `TaskCreated`
    /// and `DeadlineSet` with different field types) get separate
    /// slots without colliding in the function-level slot table.
    /// Wildcard-position bindings (`_`) are stored as `u16::MAX` and
    /// must never be read.
    pub binding_slots: std::sync::OnceLock<Vec<u16>>,
}

// `OnceLock` doesn't derive Clone (cell is invariant over T); copy
// the inner manually so the resolver's allocations survive the
// `Arc::make_mut` clones that happen during multimodule flatten.
impl Clone for MatchArm {
    fn clone(&self) -> Self {
        let binding_slots = std::sync::OnceLock::new();
        if let Some(v) = self.binding_slots.get() {
            let _ = binding_slots.set(v.clone());
        }
        Self {
            pattern: self.pattern.clone(),
            body: self.body.clone(),
            binding_slots,
        }
    }
}

impl PartialEq for MatchArm {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern && self.body == other.body
    }
}

impl MatchArm {
    /// Build a fresh arm with no binding-slot stamp yet — resolver
    /// fills `binding_slots` after slot allocation. Use this from any
    /// site that synthesises an arm (parser, AST rewrites, effect
    /// lifting, tests).
    pub fn new(pattern: Pattern, body: Spanned<Expr>) -> Self {
        Self {
            pattern,
            body: Box::new(body),
            binding_slots: std::sync::OnceLock::new(),
        }
    }

    pub fn new_boxed(pattern: Pattern, body: Box<Spanned<Expr>>) -> Self {
        Self {
            pattern,
            body,
            binding_slots: std::sync::OnceLock::new(),
        }
    }
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

/// Data for a tail-call expression.
#[derive(Debug, Clone, PartialEq)]
pub struct TailCallData {
    /// Target function name (self or mutual-recursive peer).
    pub target: String,
    /// Arguments to pass.
    pub args: Vec<Spanned<Expr>>,
}

impl TailCallData {
    pub fn new(target: String, args: Vec<Spanned<Expr>>) -> Self {
        Self { target, args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Attr(Box<Spanned<Expr>>, String),
    FnCall(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    BinOp(BinOp, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// Unary numeric negation: `-x`. Operand must be numeric (`Int` or
    /// `Float`); result is the same type. Used to be desugared in the
    /// parser to `BinOp(Sub, Literal(Int(0)), x)`, which loses the IEEE
    /// `-0.0` sign bit on `Float` operands and produces an `Int`/`Float`
    /// mixed `BinOp` that backends had to recognise with pattern hacks.
    Neg(Box<Spanned<Expr>>),
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
    /// Reuse info is populated by `ir::reuse::annotate_program_reuse`.
    TailCall(Box<TailCallData>),
    /// Independent product: `(a, b, c)!` or `(a, b, c)?!`.
    /// Elements are independent effectful expressions evaluated with no guaranteed order.
    /// `unwrap=true` (`?!`): all elements must be Result; unwraps Ok values, propagates first Err.
    /// `unwrap=false` (`!`): returns raw tuple of results.
    /// Produces a replay group (effects matched by branch_path + effect_occurrence + type + args).
    IndependentProduct(Vec<Spanned<Expr>>, bool),
    /// Compiled variable lookup: `env[last][slot]` — O(1) instead of HashMap scan.
    /// Produced by the resolver pass for locals inside function bodies.
    /// `last_use` is set by `ir::last_use` — when true, this is the final
    /// reference to this slot and backends can move instead of copy.
    Resolved {
        slot: u16,
        name: String,
        last_use: AnnotBool,
    },
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
/// so the VM can use `Vec<Value>` instead of `HashMap` lookups.
#[derive(Debug, Clone, PartialEq)]
pub struct FnResolution {
    /// Total number of local slots needed (params + bindings in body).
    pub local_count: u16,
    /// Map from local variable name → slot index in the local `Slots` frame.
    pub local_slots: std::sync::Arc<std::collections::HashMap<String, u16>>,
    /// Aver type per slot index. Length == `local_count`. Built post-
    /// typecheck so each entry pulls from the matching `Spanned::ty()`
    /// stamp on the producer expression, plus pattern-binding shape
    /// rules (`Result.Ok` → T, `Cons head` → list element, tuple item
    /// → tuple element, …). Backends that need a typed local table
    /// (the wasm-gc lowering uses one to declare each `local` with a
    /// concrete `ValType`) consume this directly instead of re-deriving
    /// the same information from patterns.
    ///
    /// Default `Type::Invalid` for unreachable / unstamped slots — every
    /// real binding gets overwritten during the slot-types pass, so an
    /// `Invalid` reaching the backend means the slot was never the
    /// target of a binding (resolver counted but no expression
    /// produced into it; usually a wildcard slot the backend skips).
    pub local_slot_types: std::sync::Arc<Vec<Type>>,
    /// Whether each slot may share an arena entry with another slot.
    /// Length == `local_count`. Set by `ir::alias::annotate_program_alias_slots`
    /// post-`last_use`. Backends that have a `mem::take`-style fast path
    /// for `Vector.set` / `Map.set` (the VM's `CALL_BUILTIN_OWNED` mask
    /// plus the fused `VECTOR_SET_OR_KEEP`) must NOT take the fast path
    /// on a flagged slot — rewriting the shared arena entry would
    /// mutate the other binding too. Wasm-gc may use it to skip
    /// clone-on-write when the slot is provably non-aliased; otherwise
    /// it falls back to `array.copy` + `array.set` on the copy.
    ///
    /// Default `false` for slots the analysis hasn't reached (anything
    /// pre-`last_use`, REPL, partial pipelines), which is the safe-but-
    /// slow choice everywhere except the VM fast path.
    pub aliased_slots: std::sync::Arc<Vec<bool>>,
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
    /// `None` for unresolved (REPL, module loading).
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
    /// Module-level effect surface declaration. `None` is legacy/mixed
    /// (no enforcement, soft warning emitted by `aver check`); `Some([])`
    /// is explicit pure; `Some([...])` is a declared boundary — every
    /// function's `! [...]` must be a subset (namespace-level entry like
    /// `Disk` admits any `Disk.*` method).
    pub effects: Option<Vec<String>>,
    pub effects_line: Option<usize>,
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
    /// Parallel to `cases`: `true` when the case was injected by
    /// `aver verify --hostile` (boundary-value expansion of a law's
    /// `given` clause), `false` for cases the user wrote directly.
    /// Empty under non-hostile runs; the renderer uses this to label
    /// failures as "outside declared given — encode as `when` if
    /// precondition" when they only fail under the hostile expansion.
    pub case_hostile_origins: Vec<bool>,
    /// Parallel to `cases`: per-case hostile effect-profile assignment
    /// for `--hostile` mode. Each inner Vec lists `(method, profile)`
    /// pairs (e.g. `("Time.now", "frozen")`) that the runner installs
    /// as oracle stubs before running the case, alongside any user-given
    /// stubs. Empty inner Vec for cases that aren't effect-hostile-
    /// expanded (declared, value-hostile-only, or fns without applicable
    /// classified effects). All entries empty under non-hostile runs.
    pub case_hostile_profiles: Vec<Vec<(String, String)>>,
    /// Parallel to `cases`: `true` when `aver verify --hostile` has
    /// injected a reverse-order twin of an earlier case. The twin
    /// shares LHS/RHS/given/profile with its forward sibling — only
    /// the execution order of independent-product branches
    /// (`(a, b)!` lowers to `CALL_PAR`) is flipped. A pure law claims
    /// its branches are independent, so the twin must produce the
    /// same result; divergence proves the claim doesn't hold under
    /// the stub map and surfaces as `verify-hostile-order-mismatch`.
    /// All entries `false` under non-hostile runs.
    pub case_reverse_order: Vec<bool>,
    pub kind: VerifyKind,
    /// Oracle v1: `trace` keyword enables trace-aware assertions
    /// (`.trace.*`, `.result`, event literals in `.contains` / match
    /// patterns). Without it, a law checks only the return value, so
    /// adding a debug print does not break proofs that do not care
    /// about traces.
    pub trace: bool,
    /// Oracle v1: `given` clauses declared at the top of a cases-form
    /// trace block. Law-form stores its givens inside `VerifyKind::Law`;
    /// cases-form doesn't have that wrapper, so this field carries them
    /// so the verify runner can build oracle-stub mappings from the
    /// same data. Empty for non-trace or law-form blocks.
    pub cases_givens: Vec<VerifyGiven>,
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
        let case_hostile_origins = vec![false; cases.len()];
        let case_hostile_profiles = vec![Vec::new(); cases.len()];
        let case_reverse_order = vec![false; cases.len()];
        Self {
            fn_name,
            line,
            cases,
            case_spans,
            case_givens: vec![],
            case_hostile_origins,
            case_hostile_profiles,
            case_reverse_order,
            kind,
            trace: false,
            cases_givens: vec![],
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
