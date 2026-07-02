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

use crate::ast::{BinOp, Expr, Literal, Spanned};

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
  for c in s.data do
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
      match names.get? i with
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

/// Parse the goal JSON emitted by [`AVER_DUMP_GOAL_ELAB`] into an
/// [`UntranslatedGoal`], or decline with an [`EngineGap`] if any node is outside
/// the closed grammar.
pub fn untranslate_goal(json: &str) -> Result<UntranslatedGoal, EngineGap> {
    let v: Value =
        serde_json::from_str(json).map_err(|e| EngineGap::new(format!("malformed goal JSON: {e}")))?;
    let mut givens: Vec<(String, String)> = Vec::new();
    let mut premises: Vec<Spanned<Expr>> = Vec::new();
    let mut cur = &v;
    // Peel the `∀`-prefix: each binder is either a data given (nameable Aver
    // type) or a Prop premise (Eq / comparison).
    loop {
        let Some(fa) = cur.get("forall") else { break };
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
        if is_prop_type(ty) {
            // A surviving hypothesis — render it as an Aver Bool expr for `when`.
            premises.push(untranslate_bool(ty)?);
        } else {
            let tn = aver_type_name(ty).ok_or_else(|| {
                EngineGap::new(format!(
                    "binder `{name}` has a type outside the grammar (cannot name it in Aver)"
                ))
            })?;
            givens.push((name.to_string(), tn));
        }
        cur = body;
    }
    // The residual claim must be an equality (the law's `lhs => rhs`).
    let (lhs, rhs) = untranslate_eq(cur)
        .ok_or_else(|| EngineGap::new("goal claim is not an equality our grammar renders"))?;
    Ok(UntranslatedGoal {
        givens,
        premises,
        claim: (lhs?, rhs?),
    })
}

/// The Aver `Bool` expression for a Prop-typed binder (`when` premise): an
/// equality becomes `a == b`, a comparison becomes `a < b` etc.
fn untranslate_bool(v: &Value) -> Result<Spanned<Expr>, EngineGap> {
    if let Some((l, r)) = untranslate_eq(v) {
        return Ok(sp(Expr::BinOp(BinOp::Eq, Box::new(l?), Box::new(r?))));
    }
    if let Some(app) = v.get("app") {
        if let Some(op) = comparison_binop(head_const(app)) {
            let args = app_operands(app, 2)?;
            return Ok(sp(Expr::BinOp(
                op,
                Box::new(untranslate_expr(&args[0])?),
                Box::new(untranslate_expr(&args[1])?),
            )));
        }
    }
    Err(EngineGap::new(
        "premise hypothesis is not an equality/comparison our grammar renders",
    ))
}

/// If `v` is `@Eq _ a b`, return the two operand JSON nodes untranslated.
#[allow(clippy::type_complexity)]
fn untranslate_eq(
    v: &Value,
) -> Option<(Result<Spanned<Expr>, EngineGap>, Result<Spanned<Expr>, EngineGap>)> {
    let app = v.get("app")?;
    if head_const(app) != Some("Eq") {
        return None;
    }
    let args = app.get("args")?.as_array()?;
    let n = args.len();
    if n < 2 {
        return None;
    }
    Some((untranslate_expr(&args[n - 2]), untranslate_expr(&args[n - 1])))
}

/// Recursive descent JSON node → `ast::Expr`, declining on any out-of-grammar
/// shape.
fn untranslate_expr(v: &Value) -> Result<Spanned<Expr>, EngineGap> {
    if let Some(nat) = v.get("nat").and_then(Value::as_str) {
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
        return untranslate_app(app);
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

fn untranslate_app(app: &Value) -> Result<Spanned<Expr>, EngineGap> {
    let head = head_const(app);
    // Numeric literal: `@OfNat.ofNat _ n _` — the middle arg carries the nat.
    if head == Some("OfNat.ofNat") {
        if let Some(args) = app.get("args").and_then(Value::as_array) {
            for a in args {
                if let Some(nat) = a.get("nat").and_then(Value::as_str) {
                    return Ok(sp(int_literal(nat)));
                }
            }
        }
        return Err(EngineGap::new("OfNat literal without a nat operand"));
    }
    // Negation: `@Neg.neg _ _ a`.
    if head == Some("Neg.neg") {
        let args = app_operands(app, 1)?;
        return Ok(sp(Expr::Neg(Box::new(untranslate_expr(&args[0])?))));
    }
    // Heterogeneous / homogeneous arithmetic — operands are the last two args.
    if let Some(op) = arith_binop(head) {
        let args = app_operands(app, 2)?;
        return Ok(sp(Expr::BinOp(
            op,
            Box::new(untranslate_expr(&args[0])?),
            Box::new(untranslate_expr(&args[1])?),
        )));
    }
    if let Some(op) = comparison_binop(head) {
        let args = app_operands(app, 2)?;
        return Ok(sp(Expr::BinOp(
            op,
            Box::new(untranslate_expr(&args[0])?),
            Box::new(untranslate_expr(&args[1])?),
        )));
    }
    if head == Some("Eq") {
        return Err(EngineGap::new("equality nested inside the claim"));
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
        args.push(untranslate_expr(a)?);
    }
    Ok(sp(Expr::FnCall(
        Box::new(sp(const_to_expr(name))),
        args,
    )))
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
        return Err(EngineGap::new("application has fewer operands than expected"));
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

/// Map a Lean surface const to its Aver surface form. Built-in `Bool`/`Int`
/// constructors we recognise map to Aver literals; everything else keeps its
/// dotted module path, un-escaping the trailing-`'` Lean reserved-word guard on
/// the final segment.
fn const_to_expr(name: &str) -> Expr {
    match name {
        "Bool.true" | "True" => return Expr::Literal(Literal::Bool(true)),
        "Bool.false" | "False" => return Expr::Literal(Literal::Bool(false)),
        _ => {}
    }
    Expr::Ident(lean_dotted_to_aver(name))
}

/// Un-escape a (possibly dotted) Lean name back to its Aver spelling: strip the
/// trailing-`'` reserved-word guard from the final segment.
fn lean_dotted_to_aver(name: &str) -> String {
    match name.rsplit_once('.') {
        Some((prefix, last)) => format!("{prefix}.{}", super::expr::lean_name_to_aver(last)),
        None => super::expr::lean_name_to_aver(name),
    }
}

/// The Aver type name for a data `∀`-binder type node (`const Int`,
/// `List X`, a user record const, …), or `None` if it cannot be named.
fn aver_type_name(v: &Value) -> Option<String> {
    if let Some(name) = v.get("const").and_then(Value::as_str) {
        return Some(match name {
            "Int" => "Int".to_string(),
            "Bool" => "Bool".to_string(),
            "String" => "Str".to_string(),
            other => lean_dotted_to_aver(other),
        });
    }
    if let Some(app) = v.get("app") {
        // `List X` → `List<X>`.
        if head_const(app) == Some("List") {
            let args = app.get("args")?.as_array()?;
            let inner = aver_type_name(args.last()?)?;
            return Some(format!("List<{inner}>"));
        }
    }
    None
}

/// True iff the binder type is a Prop we treat as a `when` premise (equality or
/// comparison), rather than a data given.
fn is_prop_type(v: &Value) -> bool {
    let Some(app) = v.get("app") else {
        return false;
    };
    let head = head_const(app);
    head == Some("Eq") || comparison_binop(head).is_some()
}

fn int_literal(nat: &str) -> Expr {
    match nat.parse::<i64>() {
        Ok(n) => Expr::Literal(Literal::Int(n)),
        // Past-i64 magnitude — keep the decimal digits (Aver `Int` is ℤ).
        Err(_) => Expr::Literal(Literal::BigInt(nat.to_string())),
    }
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
        let json = forall("a", r#"{"const":"Int"}"#, &forall("b", r#"{"const":"Int"}"#, &claim));
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(g.givens, vec![
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
        ]);
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
        let json = forall(
            "a",
            r#"{"const":"Int"}"#,
            &forall("h", &le, &claim),
        );
        let g = untranslate_goal(&json).expect("in grammar");
        assert_eq!(g.givens, vec![("a".to_string(), "Int".to_string())]);
        assert_eq!(render(&g), "given a: Int; when (0 <= a); (a + 0) => a");
    }

    #[test]
    fn user_fn_call_round_trips() {
        // ∀ (x : List<Int>), length(x) = length(x)  (const `length` applied)
        let call = |arg: &str| {
            format!(r#"{{"app":{{"fn":{{"const":"length"}},"args":[{arg}]}}}}"#)
        };
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
}
