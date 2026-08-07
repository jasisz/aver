//! `aver proof --explain`: Lean goal JSON → `ast::Expr` (the bilingual-out
//! layer). This is the INVERSE of the Aver → Lean expression translator
//! (`super::expr`), defined ONLY on the closed grammar of Expr shapes our own
//! emitter produces: literals, `BinOp` arithmetic/comparisons, `Eq` claims,
//! module-qualified function calls, and `∀`-binders. Anything outside that
//! grammar — a `match`, a lambda, a bare projection, a constructor, an
//! unrecognised head constant — is an HONEST DECLINE ([`EngineGap`]), never a
//! guess: the caller renders it as an "engine-form gap" verdict rather than a
//! misleading candidate law.
//!
//! The JSON is produced by the [`AVER_DUMP_GOAL_ELAB`] meta-tactic, appended to
//! the residual probe under `--explain` (a second, fail-soft stage of the probe
//! in `src/main/commands.rs`). The driver therefore reads a STRUCTURED goal, not
//! Lean's human-facing pretty-printer.
//!
//! Identity note (identity_guardrails): this module is a pure JSON → syntax
//! renderer. It maps Lean *surface names* back to Aver *surface names* for
//! display / re-parse; it makes no identity-routing decision and holds no
//! `FnId`/`TypeId`-keyed table, so the typed-identity invariant does not apply.

use serde_json::Value;

use crate::ast::{BinOp, Expr, Literal, Spanned, TopLevel, VerifyKind};

/// The Lean 4 meta-tactic emitted into the `--explain` residual probe file. It
/// serialises the current goal (abstracted over its local context into a closed
/// `∀`-Prop) to OUR JSON via the info log, tagged `AVER_GOAL_JSON:<fn.law>:…`.
/// Lives in the generated probe file (not `AverCommon`) so the `import Lean`
/// meta dependency is confined to the opt-in `--explain` path and never taxes a
/// normal proof build.
pub const AVER_DUMP_GOAL_ELAB: &str = r#"import Lean
open Lean Elab Tactic Meta in
private def averJsonEsc (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    out := out ++ (match c with
      | '"' => "\\\""
      | '\\' => "\\\\"
      | '\n' => "\\n"
      | '\r' => "\\r"
      | '\t' => "\\t"
      | c => toString c)
  return out
open Lean in
private partial def averExprJson (names : List String) (e : Expr) : String :=
  match e with
  | .forallE n t b _ =>
      let nm := toString n
      "{\"forall\":{\"name\":\"" ++ averJsonEsc nm ++ "\",\"ty\":" ++ averExprJson names t
        ++ ",\"body\":" ++ averExprJson (nm :: names) b ++ "}}"
  | .const n _ => "{\"const\":\"" ++ averJsonEsc (toString n) ++ "\"}"
  | .bvar i =>
      match names[i]? with
      | some nm => "{\"var\":\"" ++ averJsonEsc nm ++ "\"}"
      | none => "{\"opaque\":\"bvar\"}"
  | .lit (.natVal v) => "{\"nat\":\"" ++ toString v ++ "\"}"
  | .lit (.strVal s) => "{\"str\":\"" ++ averJsonEsc s ++ "\"}"
  | .mdata _ e => averExprJson names e
  | .app .. =>
      let fn := e.getAppFn
      let args := e.getAppArgs
      let argsJson := String.intercalate "," (args.toList.map (averExprJson names))
      "{\"app\":{\"fn\":" ++ averExprJson names fn ++ ",\"args\":[" ++ argsJson ++ "]}}"
  | .proj s i e =>
      "{\"proj\":{\"struct\":\"" ++ averJsonEsc (toString s) ++ "\",\"idx\":" ++ toString i
        ++ ",\"e\":" ++ averExprJson names e ++ "}}"
  | _ => "{\"opaque\":\"other\"}"
open Lean Elab Tactic Meta in
elab "aver_dump_goal " s:str : tactic => do
  match (← getUnsolvedGoals) with
  | [] => pure ()
  | g :: _ =>
    g.withContext do
      let ty ← instantiateMVars (← g.getType)
      let fvars ← (← getLCtx).foldlM (init := (#[] : Array Expr)) fun acc d =>
        pure (if d.isImplementationDetail then acc else acc.push d.toExpr)
      let closed ← instantiateMVars (← mkForallFVars fvars ty)
      logInfo m!"AVER_GOAL_JSON:{s.getString}:{averExprJson [] closed}"
"#;

/// The info-log marker the elaborator prefixes each dumped goal with. The driver
/// greps `lake env lean` output for lines carrying it and slices the JSON tail.
pub const GOAL_JSON_MARKER: &str = "AVER_GOAL_JSON:";

/// An honest decline: the goal expr contains a shape outside the closed grammar
/// our translator emits, so no faithful Aver law can be reconstructed. Rendered
/// as an "engine-form gap" verdict, NOT a guessed candidate.
#[derive(Debug, Clone, PartialEq)]
pub struct EngineGap {
    pub reason: String,
}

impl EngineGap {
    fn new(reason: impl Into<String>) -> Self {
        EngineGap {
            reason: reason.into(),
        }
    }
}

/// A goal un-translated back to Aver-space: the `∀`-binders split into data
/// GIVENS (name + Aver type name) and PROP premises (surviving hypotheses, e.g.
/// an induction hypothesis, rendered as Aver `Bool` expressions for `when`),
/// plus the equality CLAIM `(lhs, rhs)`.
#[derive(Debug, Clone)]
pub struct UntranslatedGoal {
    /// `(binder name, Aver type name)` for each data `∀`-binder — the law's
    /// givens.
    pub givens: Vec<(String, String)>,
    /// Surviving hypothesis binders as Aver `Bool` expressions — the law's
    /// `when` premises.
    pub premises: Vec<Spanned<Expr>>,
    /// The equality claim `(lhs, rhs)`.
    pub claim: (Spanned<Expr>, Spanned<Expr>),
}

fn sp(e: Expr) -> Spanned<Expr> {
    Spanned::new(e, 0)
}

/// Context threaded from the `--explain` render site. `peano` is set when the
/// law being un-translated has a given whose type is a canonical-Peano ADT
/// (`T { Zero; Succ(T) }`, shape-detected — any name, not just literal `Nat`).
/// The transpiler lifts such a type's VALUES to Lean `Nat` (`Zero→0`,
/// `Succ x→x + 1`), so the dumped goal speaks `Nat`. Inverting that
/// deterministic lift here — `0→T.Zero`, `x + 1 / Nat.succ x→T.Succ x`, literal
/// `n≤8→Succ^n(Zero)` — stays inside the translator image (it undoes OUR OWN
/// lift, not a guess). Nat truncated subtraction / mul / div and any non-`+1`
/// Nat arithmetic stay OUT of grammar, so the #630 boundary against foreign Nat
/// semantics is untouched. `None` (no Peano given) = exactly the pre-V2 behavior.
#[derive(Debug, Clone, Default)]
pub struct UntranslateCtx {
    pub peano: Option<PeanoCtx>,
}

/// The canonical-Peano ADT (surface type + constructor names) whose Lean-`Nat`
/// lift the un-translator inverts. Constructor names are DATA carried from the
/// law's declaration; the machinery keys on the Peano SHAPE, never on the names.
#[derive(Debug, Clone)]
pub struct PeanoCtx {
    pub type_name: String,
    pub zero_ctor: String,
    pub succ_ctor: String,
}

/// Build the [`UntranslateCtx`] for a `<fn>.<law>` under `--explain`: if the
/// law's givens include a canonical-Peano ADT (shape-detected via
/// [`detect_canonical_peano`](crate::codegen::proof_recognize::detect_canonical_peano)),
/// thread its constructor names so the un-translator can invert the `Nat` lift.
/// Name-blind: keys on the Peano SHAPE, never on the type being called `Nat`.
pub fn peano_ctx_for_law(items: &[TopLevel], fn_name: &str, law_name: &str) -> UntranslateCtx {
    let peanos: Vec<_> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::TypeDef(td) => crate::codegen::proof_recognize::detect_canonical_peano(td),
            _ => None,
        })
        .collect();
    if peanos.is_empty() {
        return UntranslateCtx::default();
    }
    for it in items {
        if let TopLevel::Verify(vb) = it
            && vb.fn_name == fn_name
            && let VerifyKind::Law(law) = &vb.kind
            && law.name == law_name
        {
            for g in &law.givens {
                for tok in type_name_tokens(&g.type_name) {
                    if let Some(p) = peanos.iter().find(|p| p.type_name == tok) {
                        return UntranslateCtx {
                            peano: Some(PeanoCtx {
                                type_name: p.type_name.clone(),
                                zero_ctor: p.base_ctor.clone(),
                                succ_ctor: p.succ_ctor.clone(),
                            }),
                        };
                    }
                }
            }
        }
    }
    UntranslateCtx::default()
}

/// The identifier tokens of a surface type string (`List<Nat>` → [`List`,
/// `Nat`]), so a Peano element type is found regardless of container nesting.
fn type_name_tokens(ty: &str) -> Vec<String> {
    ty.split(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .filter(|s| !s.is_empty())
        .map(str::to_string)
        .collect()
}

/// Parse the goal JSON emitted by [`AVER_DUMP_GOAL_ELAB`] into an
/// [`UntranslatedGoal`], or decline with an [`EngineGap`] if any node is outside
/// the closed grammar.
pub fn untranslate_goal(json: &str) -> Result<UntranslatedGoal, EngineGap> {
    untranslate_goal_ctx(json, &UntranslateCtx::default())
}

/// As [`untranslate_goal`], threading an [`UntranslateCtx`] so a canonical-Peano
/// law's `Nat`-lift is inverted (see [`UntranslateCtx`]).
pub fn untranslate_goal_ctx(
    json: &str,
    ctx: &UntranslateCtx,
) -> Result<UntranslatedGoal, EngineGap> {
    let v: Value = serde_json::from_str(json)
        .map_err(|e| EngineGap::new(format!("malformed goal JSON: {e}")))?;
    let mut givens: Vec<(String, String)> = Vec::new();
    let mut premises: Vec<Spanned<Expr>> = Vec::new();
    let mut cur = &v;
    // Peel the `∀`-prefix: each binder is a vacuous split hypothesis (dropped), a
    // Prop premise (Eq / comparison → `when`), or a data given (nameable type).
    while let Some(fa) = cur.get("forall") {
        let name = fa
            .get("name")
            .and_then(Value::as_str)
            .ok_or_else(|| EngineGap::new("forall binder without a name"))?;
        let ty = fa
            .get("ty")
            .ok_or_else(|| EngineGap::new("forall binder without a type"))?;
        let body = fa
            .get("body")
            .ok_or_else(|| EngineGap::new("forall binder without a body"))?;
        if is_vacuous_prop(ty) {
            // A trivially true (`True`, `x = x`) or contradictory (`¬(true =
            // true)`) split-branch hypothesis — drop it. A tautology premise is
            // redundant; a contradictory branch is vacuous, so dropping only
            // strengthens the residual, which the VM sample-check then judges.
        } else if is_prop_type(ty) {
            // A surviving hypothesis — render it as an Aver Bool expr for `when`.
            premises.push(untranslate_bool(ty, ctx)?);
        } else {
            let tn = aver_type_name(ty, ctx).ok_or_else(|| {
                EngineGap::new(format!(
                    "binder `{name}` has a type outside the grammar (cannot name it in Aver)"
                ))
            })?;
            givens.push((name.to_string(), tn));
        }
        cur = body;
    }
    // The residual claim must be an equality (the law's `lhs => rhs`).
    let (lhs, rhs) = untranslate_eq(cur, ctx)
        .ok_or_else(|| EngineGap::new("goal claim is not an equality our grammar renders"))?;
    Ok(UntranslatedGoal {
        givens,
        premises,
        claim: (lhs?, rhs?),
    })
}

/// The Aver `Bool` expression for a Prop-typed binder (`when` premise): an
/// equality becomes `a == b`, a comparison becomes `a < b` etc.
fn untranslate_bool(v: &Value, ctx: &UntranslateCtx) -> Result<Spanned<Expr>, EngineGap> {
    if let Some((l, r)) = untranslate_eq(v, ctx) {
        return Ok(sp(Expr::BinOp(BinOp::Eq, Box::new(l?), Box::new(r?))));
    }
    // A split else-branch hypothesis `¬(<boolExpr> = b)` (non-constant, so not
    // dropped as vacuous) → the `when`-conjunct `<boolExpr> != b`. The Bool↔Prop
    // bridge for the false side of an `if <boolFn> …` split.
    if let Some(inner) = negated_eq_inner(v)
        && let Some((l, r)) = untranslate_eq(inner, ctx)
    {
        return Ok(sp(Expr::BinOp(BinOp::Neq, Box::new(l?), Box::new(r?))));
    }
    if let Some(app) = v.get("app")
        && let Some(op) = comparison_binop(head_const(app))
    {
        require_int_carrier(app)?;
        let args = app_operands(app, 2)?;
        return Ok(sp(Expr::BinOp(
            op,
            Box::new(untranslate_expr(&args[0], ctx)?),
            Box::new(untranslate_expr(&args[1], ctx)?),
        )));
    }
    Err(EngineGap::new(
        "premise hypothesis is not an equality/comparison our grammar renders",
    ))
}

/// If `v` is `@Eq _ a b`, return the two operand JSON nodes untranslated.
#[allow(clippy::type_complexity)]
fn untranslate_eq(
    v: &Value,
    ctx: &UntranslateCtx,
) -> Option<(
    Result<Spanned<Expr>, EngineGap>,
    Result<Spanned<Expr>, EngineGap>,
)> {
    let app = v.get("app")?;
    if head_const(app) != Some("Eq") {
        return None;
    }
    let args = app.get("args")?.as_array()?;
    let n = args.len();
    if n < 2 {
        return None;
    }
    Some((
        untranslate_expr(&args[n - 2], ctx),
        untranslate_expr(&args[n - 1], ctx),
    ))
}

/// Recursive descent JSON node → `ast::Expr`, declining on any out-of-grammar
/// shape.
fn untranslate_expr(v: &Value, ctx: &UntranslateCtx) -> Result<Spanned<Expr>, EngineGap> {
    if let Some(nat) = v.get("nat").and_then(Value::as_str) {
        if let Some(p) = &ctx.peano {
            return peano_numeral_from_str(p, nat).map(sp);
        }
        return Ok(sp(int_literal(nat)));
    }
    if let Some(s) = v.get("str").and_then(Value::as_str) {
        return Ok(sp(Expr::Literal(Literal::Str(s.to_string()))));
    }
    if let Some(name) = v.get("var").and_then(Value::as_str) {
        return Ok(sp(Expr::Ident(name.to_string())));
    }
    if let Some(name) = v.get("const").and_then(Value::as_str) {
        return Ok(sp(const_to_expr(name)));
    }
    if let Some(app) = v.get("app") {
        return untranslate_app(app, ctx);
    }
    if let Some(o) = v.get("opaque").and_then(Value::as_str) {
        return Err(EngineGap::new(format!("goal contains a `{o}` node")));
    }
    if v.get("forall").is_some() {
        return Err(EngineGap::new("nested quantifier in the goal claim"));
    }
    if v.get("proj").is_some() {
        return Err(EngineGap::new("field projection outside the grammar"));
    }
    Err(EngineGap::new("unrecognised goal node"))
}

fn untranslate_app(app: &Value, ctx: &UntranslateCtx) -> Result<Spanned<Expr>, EngineGap> {
    let head = head_const(app);
    // Numeric literal: `@OfNat.ofNat _ n _` — the middle arg carries the nat.
    if head == Some("OfNat.ofNat") {
        let nat = app
            .get("args")
            .and_then(Value::as_array)
            .and_then(|args| {
                args.iter()
                    .find_map(|a| a.get("nat").and_then(Value::as_str))
            })
            .ok_or_else(|| EngineGap::new("OfNat literal without a nat operand"))?;
        // Under a canonical-Peano law, a `Nat`-carried literal is a lifted Peano
        // numeral — invert to `Succ^n(Zero)`; an `Int` literal stays as-is.
        if let Some(p) = &ctx.peano
            && carrier_const(app) == Some("Nat")
        {
            return peano_numeral_from_str(p, nat).map(sp);
        }
        return Ok(sp(int_literal(nat)));
    }
    // Canonical-Peano successor constructor `Nat.succ x` → `T.Succ(x)`.
    if head == Some("Nat.succ")
        && let Some(p) = &ctx.peano
    {
        let ops = app_operands(app, 1)?;
        return Ok(sp(peano_succ(p, untranslate_expr(&ops[0], ctx)?)));
    }
    // Negation: `@Neg.neg _ _ a` (Int only — Nat has no negation).
    if head == Some("Neg.neg") {
        require_int_carrier(app)?;
        let args = app_operands(app, 1)?;
        return Ok(sp(Expr::Neg(Box::new(untranslate_expr(&args[0], ctx)?))));
    }
    // Numeric coercions have no Aver surface: Aver `Int` is ℤ and there is no
    // `Nat`, so a `Nat`-carried value must never be laundered into an Aver
    // `Int` — that is exactly what would produce a lying sample verdict.
    if is_numeric_coercion(head) {
        return Err(EngineGap::new("numeric coercion outside the grammar"));
    }
    // Heterogeneous / homogeneous arithmetic — operands are the last two args.
    // Type-aware: only the `Int` carrier maps to an Aver operator (Nat
    // truncated subtraction differs from Aver's Int=ℤ).
    if let Some(op) = arith_binop(head) {
        // Canonical-Peano successor: `Nat`-carried `x + 1` inverts our
        // `Succ x → x + 1` value lift. Other `Nat` arithmetic (non-`+1`,
        // sub/mul/div) still declines via `require_int_carrier` below — the #630
        // boundary against foreign Nat semantics is preserved.
        if let Some(p) = &ctx.peano
            && matches!(head, Some("HAdd.hAdd") | Some("Add.add"))
            && carrier_const(app) == Some("Nat")
        {
            let ops = app_operands(app, 2)?;
            if nat_lit_value(&ops[1]) == Some(1) {
                return Ok(sp(peano_succ(p, untranslate_expr(&ops[0], ctx)?)));
            }
        }
        require_int_carrier(app)?;
        let args = app_operands(app, 2)?;
        return Ok(sp(Expr::BinOp(
            op,
            Box::new(untranslate_expr(&args[0], ctx)?),
            Box::new(untranslate_expr(&args[1], ctx)?),
        )));
    }
    if let Some(op) = comparison_binop(head) {
        require_int_carrier(app)?;
        let args = app_operands(app, 2)?;
        return Ok(sp(Expr::BinOp(
            op,
            Box::new(untranslate_expr(&args[0], ctx)?),
            Box::new(untranslate_expr(&args[1], ctx)?),
        )));
    }
    if head == Some("Eq") {
        return Err(EngineGap::new("equality nested inside the claim"));
    }
    // List builtins in our translator's image: `[x]` / `x :: xs` / `a ++ b`
    // spell back as Aver list literals and `List.concat`.
    if head == Some("List.nil") {
        return Ok(sp(Expr::List(vec![])));
    }
    if head == Some("List.cons") {
        // `@List.cons _ a b` → `List.concat([a], b)`.
        let ops = app_operands(app, 2)?;
        let a = untranslate_expr(&ops[0], ctx)?;
        let b = untranslate_expr(&ops[1], ctx)?;
        return Ok(sp(Expr::FnCall(
            Box::new(sp(Expr::Ident("List.concat".to_string()))),
            vec![sp(Expr::List(vec![a])), b],
        )));
    }
    if matches!(
        head,
        Some("HAppend.hAppend") | Some("Append.append") | Some("List.append")
    ) {
        // `a ++ b` → `List.concat(a, b)`.
        let ops = app_operands(app, 2)?;
        return Ok(sp(Expr::FnCall(
            Box::new(sp(Expr::Ident("List.concat".to_string()))),
            vec![
                untranslate_expr(&ops[0], ctx)?,
                untranslate_expr(&ops[1], ctx)?,
            ],
        )));
    }
    // Otherwise: a user / module function applied to arguments. The head must be
    // a plain const (a higher-order applied variable is out of grammar).
    let Some(name) = head else {
        return Err(EngineGap::new(
            "application head is not a named function (higher-order)",
        ));
    };
    let all = app
        .get("args")
        .and_then(Value::as_array)
        .ok_or_else(|| EngineGap::new("application without args"))?;
    let mut args = Vec::with_capacity(all.len());
    for a in all {
        args.push(untranslate_expr(a, ctx)?);
    }
    Ok(sp(Expr::FnCall(Box::new(sp(const_to_expr(name))), args)))
}

/// The head constant name of an `app` JSON node, if the head is a `const`.
fn head_const(app: &Value) -> Option<&str> {
    app.get("fn")?.get("const")?.as_str()
}

/// The last `n` args of an application (operands), declining if there are fewer.
fn app_operands(app: &Value, n: usize) -> Result<Vec<Value>, EngineGap> {
    let args = app
        .get("args")
        .and_then(Value::as_array)
        .ok_or_else(|| EngineGap::new("application without args"))?;
    if args.len() < n {
        return Err(EngineGap::new(
            "application has fewer operands than expected",
        ));
    }
    Ok(args[args.len() - n..].to_vec())
}

fn arith_binop(head: Option<&str>) -> Option<BinOp> {
    match head? {
        "HAdd.hAdd" | "Add.add" => Some(BinOp::Add),
        "HSub.hSub" | "Sub.sub" => Some(BinOp::Sub),
        "HMul.hMul" | "Mul.mul" => Some(BinOp::Mul),
        "HDiv.hDiv" | "Div.div" => Some(BinOp::Div),
        _ => None,
    }
}

fn comparison_binop(head: Option<&str>) -> Option<BinOp> {
    match head? {
        "LT.lt" => Some(BinOp::Lt),
        "LE.le" => Some(BinOp::Lte),
        "GT.gt" => Some(BinOp::Gt),
        "GE.ge" => Some(BinOp::Gte),
        "Ne" => Some(BinOp::Neq),
        _ => None,
    }
}

/// Numeric coercion heads (`↑`, `Int.ofNat`, …). These bridge Lean's `Nat`/`Int`
/// hierarchy, which Aver does not have — there is no faithful Aver surface, so
/// the un-translator declines rather than dropping the coercion silently.
fn is_numeric_coercion(head: Option<&str>) -> bool {
    matches!(
        head,
        Some(
            "Nat.cast"
                | "NatCast.natCast"
                | "IntCast.intCast"
                | "Int.ofNat"
                | "Int.cast"
                | "Int.toNat"
        )
    )
}

/// Gate an arithmetic / comparison / negation application to the `Int` carrier
/// our operators are sound for. Lean elaborates these with the carrier type as
/// the FIRST argument (`@HAdd.hAdd α β γ …`, `@LT.lt α …`, `@Neg.neg α …`).
/// Aver has no `Nat` and Aver `Int` is ℤ, so mapping Nat arithmetic to an Aver
/// operator would misread Nat truncated subtraction as real subtraction and
/// invent a counterexample for a goal that is TRUE in Lean — decline instead.
fn require_int_carrier(app: &Value) -> Result<(), EngineGap> {
    match app
        .get("args")
        .and_then(Value::as_array)
        .and_then(|a| a.first())
        .and_then(|t| t.get("const"))
        .and_then(Value::as_str)
    {
        Some("Int") => Ok(()),
        Some("Nat") => Err(EngineGap::new(
            "natural-number arithmetic (truncated subtraction differs from Aver's Int)",
        )),
        _ => Err(EngineGap::new(
            "arithmetic over a non-Int carrier type outside the grammar",
        )),
    }
}

/// Map a Lean surface const to its Aver surface form. Built-in `Bool`/`Int`
/// constructors we recognise map to Aver literals; everything else keeps its
/// dotted module path, un-escaping the trailing-`'` Lean reserved-word guard on
/// the final segment.
fn const_to_expr(name: &str) -> Expr {
    match name {
        "Bool.true" | "True" => return Expr::Literal(Literal::Bool(true)),
        "Bool.false" | "False" => return Expr::Literal(Literal::Bool(false)),
        "List.nil" => return Expr::List(vec![]),
        _ => {}
    }
    Expr::Ident(lean_dotted_to_aver(name))
}

/// Un-escape a (possibly dotted) Lean name back to its Aver spelling: strip the
/// trailing-`'` reserved-word guard from every segment (module and type
/// segments carry the guard too — `Models.Type'` is Aver's `Models.Type`).
fn lean_dotted_to_aver(name: &str) -> String {
    name.split('.')
        .map(super::expr::lean_name_to_aver)
        .collect::<Vec<_>>()
        .join(".")
}

/// The Aver type name for a data `∀`-binder type node (`const Int`,
/// `List X`, a user record const, …), or `None` if it cannot be named. Under a
/// canonical-Peano law the lifted `Nat` maps back to the ADT's surface name `T`
/// (identity when the ADT is literally called `Nat`, as in the witnesses).
fn aver_type_name(v: &Value, ctx: &UntranslateCtx) -> Option<String> {
    if let Some(name) = v.get("const").and_then(Value::as_str) {
        return Some(match name {
            "Int" => "Int".to_string(),
            "Bool" => "Bool".to_string(),
            "String" => "Str".to_string(),
            "Nat" => match &ctx.peano {
                Some(p) => p.type_name.clone(),
                None => lean_dotted_to_aver("Nat"),
            },
            other => lean_dotted_to_aver(other),
        });
    }
    if let Some(app) = v.get("app") {
        // `List X` → `List<X>`.
        if head_const(app) == Some("List") {
            let args = app.get("args")?.as_array()?;
            let inner = aver_type_name(args.last()?, ctx)?;
            return Some(format!("List<{inner}>"));
        }
    }
    None
}

/// True iff the binder type is a Prop we treat as a `when` premise: an equality,
/// a comparison, or a negated equality (`¬(a = b)`, a split else-branch).
fn is_prop_type(v: &Value) -> bool {
    let Some(app) = v.get("app") else {
        return false;
    };
    let head = head_const(app);
    head == Some("Eq") || comparison_binop(head).is_some() || negated_eq_inner(v).is_some()
}

/// If `v` is `Not <inner>` with `<inner>` an `@Eq …`, return the inner Eq node.
fn negated_eq_inner(v: &Value) -> Option<&Value> {
    let app = v.get("app")?;
    if head_const(app) != Some("Not") {
        return None;
    }
    let inner = app.get("args")?.as_array()?.last()?;
    if inner.get("app").map(head_const)? == Some("Eq") {
        Some(inner)
    } else {
        None
    }
}

fn int_literal(nat: &str) -> Expr {
    match nat.parse::<i64>() {
        Ok(n) => Expr::Literal(Literal::Int(n)),
        // Past-i64 magnitude — keep the decimal digits (Aver `Int` is ℤ).
        Err(_) => Expr::Literal(Literal::BigInt(nat.to_string())),
    }
}

/// A `∀`-binder whose Prop is trivially true (`True`, `x = x`) or the negation
/// of one (`¬(x = x)` — a contradictory split branch). Dropped rather than kept
/// as a `when`: a tautology premise is redundant, a contradictory branch is
/// vacuous, so either way the residual is at worst strengthened and the VM
/// sample-check judges it. Not gated on the Peano ctx — it is a general cleanup.
fn is_vacuous_prop(v: &Value) -> bool {
    if v.get("const").and_then(Value::as_str) == Some("True") {
        return true;
    }
    let Some(app) = v.get("app") else {
        return false;
    };
    match head_const(app) {
        Some("Eq") => eq_operands_identical(app),
        Some("Not") => app
            .get("args")
            .and_then(Value::as_array)
            .and_then(|a| a.last())
            .is_some_and(is_vacuous_prop),
        _ => false,
    }
}

/// `@Eq _ a b` whose two operands are structurally identical JSON (`a = a`).
fn eq_operands_identical(app: &Value) -> bool {
    match app.get("args").and_then(Value::as_array) {
        Some(args) if args.len() >= 2 => args[args.len() - 2] == args[args.len() - 1],
        _ => false,
    }
}

/// The literal `Nat` value of a node that is a bare `{"nat":n}` or an
/// `@OfNat.ofNat _ n _` (used to recognise the `+1` successor), else `None`.
fn nat_lit_value(v: &Value) -> Option<u64> {
    if let Some(n) = v.get("nat").and_then(Value::as_str) {
        return n.parse().ok();
    }
    let app = v.get("app")?;
    if head_const(app) != Some("OfNat.ofNat") {
        return None;
    }
    app.get("args")?
        .as_array()?
        .iter()
        .find_map(|a| a.get("nat").and_then(Value::as_str))
        .and_then(|n| n.parse().ok())
}

/// The carrier type const of an `@Op α …` application (its first argument).
fn carrier_const(app: &Value) -> Option<&str> {
    app.get("args")?.as_array()?.first()?.get("const")?.as_str()
}

/// The Peano base value `T.Zero`.
fn peano_zero(p: &PeanoCtx) -> Expr {
    Expr::Ident(format!("{}.{}", p.type_name, p.zero_ctor))
}

/// The Peano successor `T.Succ(x)`.
fn peano_succ(p: &PeanoCtx, x: Spanned<Expr>) -> Expr {
    Expr::FnCall(
        Box::new(sp(Expr::Ident(format!("{}.{}", p.type_name, p.succ_ctor)))),
        vec![x],
    )
}

/// The Peano numeral `Succ^n(Zero)`; declines past 8 (nobody wants `Succ^300`).
fn peano_numeral(p: &PeanoCtx, n: u64) -> Result<Expr, EngineGap> {
    if n > 8 {
        return Err(EngineGap::new(
            "Peano numeral larger than 8 — declines rather than nesting Succ",
        ));
    }
    let mut e = peano_zero(p);
    for _ in 0..n {
        e = peano_succ(p, sp(e));
    }
    Ok(e)
}

/// Parse a decimal string as a Peano numeral (see [`peano_numeral`]).
fn peano_numeral_from_str(p: &PeanoCtx, nat: &str) -> Result<Expr, EngineGap> {
    let n: u64 = nat
        .parse()
        .map_err(|_| EngineGap::new("Peano numeral is not a natural number"))?;
    peano_numeral(p, n)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::unparse;

    /// Render an untranslated goal to a one-line Aver law body for assertions.
    fn render(g: &UntranslatedGoal) -> String {
        let mut out = String::new();
        for (n, t) in &g.givens {
            out.push_str(&format!("given {n}: {t}; "));
        }
        for p in &g.premises {
            let mut buf = String::new();
            unparse::write_expr_public(&mut buf, p, 0).unwrap();
            out.push_str(&format!("when {buf}; "));
        }
        let mut l = String::new();
        let mut r = String::new();
        unparse::write_expr_public(&mut l, &g.claim.0, 0).unwrap();
        unparse::write_expr_public(&mut r, &g.claim.1, 0).unwrap();
        out.push_str(&format!("{l} => {r}"));
        out
    }

    // `@Eq Int (@HAdd.hAdd Int Int Int i a b) (@HAdd.hAdd Int Int Int i b a)`
    // — the elaborated form of `a + b = b + a` over Int.
    fn eq_json(lhs: &str, rhs: &str) -> String {
        format!(r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Int"}},{lhs},{rhs}]}}}}"#)
    }
    fn hadd(a: &str, b: &str) -> String {
        format!(
            r#"{{"app":{{"fn":{{"const":"HAdd.hAdd"}},"args":[{{"const":"Int"}},{{"const":"Int"}},{{"const":"Int"}},{{"opaque":"other"}},{a},{b}]}}}}"#
        )
    }
    fn var(n: &str) -> String {
        format!(r#"{{"var":"{n}"}}"#)
    }
    fn forall(name: &str, ty: &str, body: &str) -> String {
        format!(r#"{{"forall":{{"name":"{name}","ty":{ty},"body":{body}}}}}"#)
    }

    #[test]
    fn round_trips_int_commutativity() {
        // ∀ (a : Int) (b : Int), a + b = b + a
        let claim = eq_json(&hadd(&var("a"), &var("b")), &hadd(&var("b"), &var("a")));
        let json = forall(
            "a",
            r#"{"const":"Int"}"#,
            &forall("b", r#"{"const":"Int"}"#, &claim),
        );
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(
            g.givens,
            vec![
                ("a".to_string(), "Int".to_string()),
                ("b".to_string(), "Int".to_string()),
            ]
        );
        assert!(g.premises.is_empty());
        assert_eq!(render(&g), "given a: Int; given b: Int; (a + b) => (b + a)");
    }

    #[test]
    fn premise_becomes_when() {
        // ∀ (a : Int), (0 <= a) → a + 0 = a  (the `<=` binder is a `when`)
        let le = format!(
            r#"{{"app":{{"fn":{{"const":"LE.le"}},"args":[{{"const":"Int"}},{{"opaque":"other"}},{},{}]}}}}"#,
            r#"{"nat":"0"}"#,
            var("a")
        );
        let claim = eq_json(&hadd(&var("a"), r#"{"nat":"0"}"#), &var("a"));
        let json = forall("a", r#"{"const":"Int"}"#, &forall("h", &le, &claim));
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(g.givens, vec![("a".to_string(), "Int".to_string())]);
        assert_eq!(render(&g), "given a: Int; when (0 <= a); (a + 0) => a");
    }

    #[test]
    fn user_fn_call_round_trips() {
        // ∀ (x : List<Int>), length(x) = length(x)  (const `length` applied)
        let call = |arg: &str| format!(r#"{{"app":{{"fn":{{"const":"length"}},"args":[{arg}]}}}}"#);
        let list_ty = r#"{"app":{"fn":{"const":"List"},"args":[{"const":"Int"}]}}"#;
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Nat"}},{},{}]}}}}"#,
            call(&var("x")),
            call(&var("x"))
        );
        let json = forall("x", list_ty, &claim);
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(g.givens, vec![("x".to_string(), "List<Int>".to_string())]);
        assert_eq!(render(&g), "given x: List<Int>; length(x) => length(x)");
    }

    #[test]
    fn residual_maps_list_builtins_and_ih_premise() {
        // The prop_07 (`length (qrev …)`) cons-arm residual shape, with the
        // trailing arithmetic Int-carried so the grammar renders end-to-end
        // (the real Nat-carried prop_07 declines — see
        // `declines_nat_subtraction`): List.cons / List.nil / ++ must spell
        // back through Aver `List.concat` + literals, and the induction
        // hypothesis binder becomes a `when`.
        let list_int = r#"{"app":{"fn":{"const":"List"},"args":[{"const":"Int"}]}}"#;
        let inst = r#"{"opaque":"inst"}"#;
        let length = |a: &str| format!(r#"{{"app":{{"fn":{{"const":"length"}},"args":[{a}]}}}}"#);
        let qrev =
            |a: &str, b: &str| format!(r#"{{"app":{{"fn":{{"const":"qrev"}},"args":[{a},{b}]}}}}"#);
        let plus =
            |a: &str, b: &str| format!(r#"{{"app":{{"fn":{{"const":"plus"}},"args":[{a},{b}]}}}}"#);
        let nil =
            format!(r#"{{"app":{{"fn":{{"const":"List.nil"}},"args":[{{"const":"Int"}}]}}}}"#);
        // `[] ++ y`
        let append = format!(
            r#"{{"app":{{"fn":{{"const":"HAppend.hAppend"}},"args":[{list_int},{list_int},{list_int},{inst},{nil},{}]}}}}"#,
            var("y")
        );
        // `head :: ([] ++ y)`
        let cons = format!(
            r#"{{"app":{{"fn":{{"const":"List.cons"}},"args":[{{"const":"Int"}},{},{append}]}}}}"#,
            var("head")
        );
        // `plus (length tail) (length y) + 1`
        let one = format!(
            r#"{{"app":{{"fn":{{"const":"OfNat.ofNat"}},"args":[{{"const":"Nat"}},{{"nat":"1"}},{inst}]}}}}"#
        );
        let rhs = format!(
            r#"{{"app":{{"fn":{{"const":"HAdd.hAdd"}},"args":[{{"const":"Int"}},{{"const":"Int"}},{{"const":"Int"}},{inst},{},{one}]}}}}"#,
            plus(&length(&var("tail")), &length(&var("y")))
        );
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Int"}},{},{rhs}]}}}}"#,
            length(&qrev(&var("tail"), &cons))
        );
        let ih_ty = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Int"}},{},{}]}}}}"#,
            length(&qrev(&var("tail"), &var("y"))),
            plus(&length(&var("tail")), &length(&var("y")))
        );
        let json = forall(
            "y",
            list_int,
            &forall(
                "head",
                r#"{"const":"Int"}"#,
                &forall("tail", list_int, &forall("ih", &ih_ty, &claim)),
            ),
        );
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(
            g.givens,
            vec![
                ("y".to_string(), "List<Int>".to_string()),
                ("head".to_string(), "Int".to_string()),
                ("tail".to_string(), "List<Int>".to_string()),
            ]
        );
        assert_eq!(g.premises.len(), 1);
        let r = render(&g);
        assert!(
            r.contains("when (length(qrev(tail, y)) == plus(length(tail), length(y)))"),
            "{r}"
        );
        assert!(r.contains("List.concat([head], List.concat([], y))"), "{r}");
        assert!(r.ends_with("=> (plus(length(tail), length(y)) + 1)"), "{r}");
        assert!(!r.contains("List.cons") && !r.contains("hAppend"), "{r}");
    }

    #[test]
    fn declines_out_of_grammar_lambda() {
        // A claim whose operand is an opaque (e.g. lambda) node → engine gap.
        let claim = eq_json(r#"{"opaque":"other"}"#, &var("a"));
        let json = forall("a", r#"{"const":"Int"}"#, &claim);
        let err = untranslate_goal(&json).expect_err("out of grammar");
        assert!(err.reason.contains("other"), "{}", err.reason);
    }

    #[test]
    fn declines_non_equality_claim() {
        // Claim is a bare comparison, not an equality.
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"LE.le"}},"args":[{{"const":"Int"}},{{"opaque":"other"}},{},{}]}}}}"#,
            var("a"),
            var("a")
        );
        let json = forall("a", r#"{"const":"Int"}"#, &claim);
        let err = untranslate_goal(&json).expect_err("not an equality");
        assert!(err.reason.contains("equality"), "{}", err.reason);
    }

    #[test]
    fn declines_nat_subtraction() {
        // ∀ (a b : Nat), (a - b) + b = a. In Nat this is FALSE→saturating-true
        // territory (2 - 5 = 0), but mapping `HSub.hSub Nat` to Aver's `-`
        // (Int=ℤ) would invent a counterexample for a goal true in Lean, so the
        // Nat carrier must decline rather than untranslate the subtraction.
        let hsub = format!(
            r#"{{"app":{{"fn":{{"const":"HSub.hSub"}},"args":[{{"const":"Nat"}},{{"const":"Nat"}},{{"const":"Nat"}},{{"opaque":"inst"}},{},{}]}}}}"#,
            var("a"),
            var("b")
        );
        let lhs = format!(
            r#"{{"app":{{"fn":{{"const":"HAdd.hAdd"}},"args":[{{"const":"Nat"}},{{"const":"Nat"}},{{"const":"Nat"}},{{"opaque":"inst"}},{hsub},{}]}}}}"#,
            var("b")
        );
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Nat"}},{lhs},{}]}}}}"#,
            var("a")
        );
        let json = forall(
            "a",
            r#"{"const":"Nat"}"#,
            &forall("b", r#"{"const":"Nat"}"#, &claim),
        );
        let err = untranslate_goal(&json).expect_err("Nat arithmetic must decline");
        assert!(err.reason.contains("natural-number"), "{}", err.reason);
    }

    // ---- V2: Peano-inverse grammar (Wall A) + vacuous split-binder drop ----
    //
    // These feed the SALVAGED krok-0 dumps (`testdata/lemma_calc_krok0/*.json`,
    // copied verbatim from the probe run) through the extended grammar. The hard
    // gate is `peano_66_1_*`: p66_1 must reconstruct the FORCED lemma
    // `when le(a,b); le(a, Nat.S(b)) => true` (modulo naming). Under the
    // witnesses the Peano ADT is `type Nat { Z; S(Nat) }`.

    const DUMP_P66_0: &str = include_str!("testdata/lemma_calc_krok0/p66_0.json");
    const DUMP_P66_1: &str = include_str!("testdata/lemma_calc_krok0/p66_1.json");
    const DUMP_P66_2: &str = include_str!("testdata/lemma_calc_krok0/p66_2.json");
    const DUMP_P73_0: &str = include_str!("testdata/lemma_calc_krok0/p73_0.json");
    const DUMP_P73_1: &str = include_str!("testdata/lemma_calc_krok0/p73_1.json");

    /// The `type Nat { Z; S(Nat) }` Peano context the p66/p73 witnesses declare.
    fn peano_nat() -> UntranslateCtx {
        UntranslateCtx {
            peano: Some(PeanoCtx {
                type_name: "Nat".to_string(),
                zero_ctor: "Z".to_string(),
                succ_ctor: "S".to_string(),
            }),
        }
    }

    #[test]
    fn peano_66_1_forces_successor_lemma() {
        // THE STEP-0 GATE. p66_1 (the `isZ`-false branch, `len + 1`) must come
        // back as the forced conditional lemma with `len xs + 1` inverted to
        // `Nat.S(len xs)` and the contradictory `¬(true = true)` split binder
        // dropped — leaving exactly the IH as the single `when`.
        let g = untranslate_goal_ctx(DUMP_P66_1, &peano_nat()).expect("in grammar under Peano");
        assert_eq!(
            g.givens,
            vec![("tail".to_string(), "List<Nat>".to_string())]
        );
        assert_eq!(g.premises.len(), 1, "only the IH survives as `when`");
        assert_eq!(
            render(&g),
            "given tail: List<Nat>; \
             when (le(len(filterZ(tail)), len(tail)) == true); \
             le(len(filterZ(tail)), Nat.S(len(tail))) => true"
        );
    }

    #[test]
    fn peano_66_1_without_ctx_still_declines_nat_arithmetic() {
        // Wall A is real: WITHOUT the Peano ctx, the same dump declines on the
        // `Nat`-carried `+ 1` (the #630 boundary). The ctx is what unlocks it.
        let err = untranslate_goal(DUMP_P66_1).expect_err("Nat `+1` declines without ctx");
        assert!(err.reason.contains("natural-number"), "{}", err.reason);
    }

    #[test]
    fn peano_66_0_drops_vacuous_binder_and_stays_in_grammar() {
        // p66_0 (the `isZ`-true branch) carries a trivially-true `true = true`
        // split binder — dropped — and no `+1`; it stays fully in grammar.
        let g = untranslate_goal_ctx(DUMP_P66_0, &peano_nat()).expect("in grammar under Peano");
        assert_eq!(
            g.givens,
            vec![("tail".to_string(), "List<Nat>".to_string())]
        );
        assert_eq!(g.premises.len(), 1, "IH survives, vacuous binder dropped");
        let r = render(&g);
        assert!(
            r.ends_with("le(len(List.concat([], filterZ(tail))), len(tail)) => true"),
            "{r}"
        );
        assert!(!r.contains("hAppend") && !r.contains("List.cons"), "{r}");
    }

    #[test]
    fn peano_66_2_declines_on_blocked_match() {
        // p66_2 still has the unreduced blocked `if isZ … then … else …`: its
        // `ite` decidability condition (`false = true`) is a nested equality and
        // its `isZ.match_1` args are opaque (Wall B, cleaned in step 1's Lean-side
        // re-strip). Descent hits the nested-`Eq` condition first — honest decline.
        let err = untranslate_goal_ctx(DUMP_P66_2, &peano_nat()).expect_err("blocked match");
        assert!(err.reason.contains("equality nested"), "{}", err.reason);
    }

    #[test]
    fn peano_73_1_declines_on_blocked_match() {
        // p73_1 likewise carries the unreduced `ite`/`isZ.match_1` residual.
        let err = untranslate_goal_ctx(DUMP_P73_1, &peano_nat()).expect_err("blocked match");
        assert!(err.reason.contains("equality nested"), "{}", err.reason);
    }

    #[test]
    fn peano_73_0_in_grammar_but_still_carries_uncleaned_ite() {
        // p73_0's split condition already collapsed to `True`, so there is no
        // opaque node — but the `ite` shell survives (Wall B), so the residual
        // is a still-uncleaned candidate. Step 1's reduceIte re-strip removes it;
        // documented here as the honest step-0 measurement, not a gate.
        let g = untranslate_goal_ctx(DUMP_P73_0, &peano_nat()).expect("in grammar (messy)");
        let r = render(&g);
        assert!(r.contains("ite("), "expected uncleaned ite shell: {r}");
        // The `[Z]` singleton (`OfNat 0`) inverted to the Peano base `Nat.Z`.
        assert!(r.contains("Nat.Z"), "{r}");
    }

    #[test]
    fn peano_numeral_nests_succ() {
        // A bare `Nat`-carried literal `2` inverts to `Nat.S(Nat.S(Nat.Z))`.
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Nat"}},{},{}]}}}}"#,
            var("x"),
            ofnat_nat("2"),
        );
        let json = forall("x", r#"{"const":"Nat"}"#, &claim);
        let g = untranslate_goal_ctx(&json, &peano_nat()).expect("in grammar");
        assert_eq!(render(&g), "given x: Nat; x => Nat.S(Nat.S(Nat.Z))");
    }

    #[test]
    fn peano_numeral_over_eight_declines() {
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Nat"}},{},{}]}}}}"#,
            var("x"),
            ofnat_nat("9"),
        );
        let json = forall("x", r#"{"const":"Nat"}"#, &claim);
        let err = untranslate_goal_ctx(&json, &peano_nat()).expect_err("too large");
        assert!(err.reason.contains("larger than 8"), "{}", err.reason);
    }

    fn ofnat_nat(n: &str) -> String {
        format!(
            r#"{{"app":{{"fn":{{"const":"OfNat.ofNat"}},"args":[{{"const":"Nat"}},{{"nat":"{n}"}},{{"opaque":"inst"}}]}}}}"#
        )
    }

    #[test]
    fn negated_bool_equality_becomes_neq_when() {
        // A non-constant split else-branch `¬(f x = true)` → `when (f(x) != true)`
        // (the false side of an `if f x …` split); vacuous `¬(true = true)` still
        // drops via `is_vacuous_prop`, so only genuine hypotheses reach here.
        let f = |a: &str| format!(r#"{{"app":{{"fn":{{"const":"f"}},"args":[{a}]}}}}"#);
        let not_eq = format!(
            r#"{{"app":{{"fn":{{"const":"Not"}},"args":[{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Bool"}},{},{{"const":"Bool.true"}}]}}}}]}}}}"#,
            f(&var("x"))
        );
        let claim = format!(
            r#"{{"app":{{"fn":{{"const":"Eq"}},"args":[{{"const":"Bool"}},{},{{"const":"Bool.true"}}]}}}}"#,
            f(&var("x"))
        );
        let json = forall("x", r#"{"const":"Int"}"#, &forall("h", &not_eq, &claim));
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(g.givens, vec![("x".to_string(), "Int".to_string())]);
        assert_eq!(g.premises.len(), 1);
        assert_eq!(
            render(&g),
            "given x: Int; when (f(x) != true); f(x) => true"
        );
    }

    #[test]
    fn type_name_tokens_splits_containers() {
        assert_eq!(type_name_tokens("List<Nat>"), vec!["List", "Nat"]);
        assert_eq!(type_name_tokens("Nat"), vec!["Nat"]);
        assert_eq!(type_name_tokens("Map<Str, Int>"), vec!["Map", "Str", "Int"]);
    }

    #[test]
    fn peano_ctx_detected_from_real_prop_66_source() {
        // End-to-end for the ctx constructor: parse the real witness and confirm
        // the `filterLenLe` law's `List<Nat>` given resolves to the shape-detected
        // Peano ADT with its actual `Z`/`S` constructor names (name-blind: the
        // detector keyed on shape, the names are carried as data).
        let src = include_str!("../../../proof-corpus/tip/isaplanner-mono/prop_66.av");
        let items = crate::source::parse_source(src).expect("prop_66 parses");
        let ctx = peano_ctx_for_law(&items, "filterZ", "filterLenLe");
        let p = ctx.peano.expect("filterLenLe has a List<Nat> given");
        assert_eq!(p.type_name, "Nat");
        assert_eq!(p.zero_ctor, "Z");
        assert_eq!(p.succ_ctor, "S");
    }

    #[test]
    fn peano_ctx_absent_when_no_peano_given() {
        // A law over Int only → no Peano ctx (pre-V2 behavior preserved).
        let src = "fn f(x: Int) -> Int\n    x\n\n\
                   verify f law idem\n        given x: Int = 1..3\n        f(x) => f(x)\n";
        let items = crate::source::parse_source(src).expect("parses");
        let ctx = peano_ctx_for_law(&items, "f", "idem");
        assert!(ctx.peano.is_none());
    }
}
