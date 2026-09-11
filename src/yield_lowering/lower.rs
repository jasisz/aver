//! Per-function lowering: the body of one `yield` function becomes a
//! segment graph. A segment is straight-line code that ends at a stop
//! (`Waiting(...)`), at the function's result (`Done(...)`), or at a
//! branch whose arms continue into other segments. Every segment lives
//! in a pure generated function; the state a stop carries is the set of
//! variables the rest of its control path still reads.

use std::collections::{HashMap, HashSet, VecDeque};

use super::build::*;
use crate::ast::*;
use crate::codegen::expr_walk;
use crate::types::checker::TypeError;

/// Generated spellings for one lowered function `f`: `__fStart`,
/// `__fAnswer<Kind>`, `__F<Kind>State`, `__FRequest`, `__FOutcome`.
pub(super) struct Names {
    lower: String,
    upper: String,
}

impl Names {
    pub(super) fn new(fn_name: &str) -> Self {
        Self {
            lower: format!("__{fn_name}"),
            upper: format!("__{}", capitalize(fn_name)),
        }
    }
    pub(super) fn start(&self) -> String {
        format!("{}Start", self.lower)
    }
    pub(super) fn answer(&self, kind: &str) -> String {
        format!("{}Answer{kind}", self.lower)
    }
    pub(super) fn state(&self, kind: &str) -> String {
        format!("{}{kind}State", self.upper)
    }
    pub(super) fn request(&self) -> String {
        format!("{}Request", self.upper)
    }
    pub(super) fn outcome(&self) -> String {
        format!("{}Outcome", self.upper)
    }
    fn after(&self, variant: &str) -> String {
        format!("{}After{variant}", self.lower)
    }
    fn join(&self, n: usize) -> String {
        format!("{}Join{n}", self.lower)
    }
}

/// The kind of the self tail call, and the one kind name no operation may
/// be given: `__fAnswerYield` re-enters the function, it does not answer a
/// capability, and the two carry different data.
const YIELD_KIND: &str = "Yield";

/// One request kind: an operation the function stops on, or `Yield`.
struct Kind {
    name: String,
    /// The operation's declared argument types; for `Yield`, empty.
    arg_types: Vec<String>,
    /// The operation's result type; `Yield` carries no answer.
    answer_type: Option<String>,
    variants: Vec<Variant>,
}

/// One stop of a kind: its live variables and what runs after the answer.
struct Variant {
    name: String,
    fields: Vec<(String, String)>,
    arm: Spanned<Expr>,
}

/// Where the value of the expression being lowered goes.
enum Ret {
    /// It is the function's result: `Done(v)`.
    Done,
    /// It is the argument of a generated continuation function.
    Join {
        name: String,
        live: Vec<String>,
        takes_value: bool,
    },
    /// The (already lowered, statement-free) rest follows in place, with
    /// no binder: nothing built this join over a bound value, so `body`
    /// never needs to see one substituted in.
    Inline { body: Spanned<Expr> },
}

impl Ret {
    fn live_names(&self) -> Vec<String> {
        match self {
            Ret::Done => Vec::new(),
            Ret::Join { live, .. } => live.clone(),
            Ret::Inline { body } => {
                let mut out = HashSet::new();
                free_idents(body, &HashSet::new(), &mut out);
                out.into_iter().collect()
            }
        }
    }
}

struct Segment {
    stmts: Vec<Stmt>,
    tail: Spanned<Expr>,
}

/// The rest of a path after a cut, lowered, with the variables it reads
/// from before the cut (name and type, in scope order) and whether it
/// reads the value the cut binds.
struct Continuation {
    fields: Vec<(String, String)>,
    segment: Segment,
    uses_bind: bool,
}

pub(super) struct Generated {
    /// The protocol a coordinator refers to: types, `Start`, answer functions.
    pub public_names: Vec<String>,
    pub items: Vec<TopLevel>,
    /// The same protocol as data, for the generator of the loop.
    pub protocol: super::ProcessProtocol,
}

pub(super) fn lower_fn(
    fd: &FnDef,
    marked: &crate::config::MarkedCapabilities,
    fn_sigs: &super::FnSigs,
) -> Result<Generated, Vec<TypeError>> {
    let mut lowering = Lowering::new(fd, marked, fn_sigs);
    match lowering.run() {
        Ok(generated) => Ok(generated),
        Err(()) => Err(lowering.errors),
    }
}

struct Lowering<'a> {
    fd: &'a FnDef,
    names: Names,
    /// The capabilities this program answers itself. A call to an operation
    /// of one of them is a stop; every other call runs in place. Decided by
    /// the manifest, not by the function's effect list (proposal §3.1).
    marked: &'a crate::config::MarkedCapabilities,
    /// Declared effects per function of the program as written, so a
    /// generated function can carry exactly the in-place effects its own
    /// segment performs (decision 4).
    fn_sigs: &'a super::FnSigs,
    /// The function's own effect list minus `yield`: what an in-place
    /// effect call looks like.
    effect_entries: Vec<String>,
    /// Canonical operation → request kind name, disambiguated up front.
    kind_names: HashMap<String, String>,
    kinds: Vec<Kind>,
    helpers: Vec<FnDef>,
    errors: Vec<TypeError>,
    tmp_counter: usize,
    stop_counter: usize,
    join_counter: usize,
}

impl<'a> Lowering<'a> {
    fn new(
        fd: &'a FnDef,
        marked: &'a crate::config::MarkedCapabilities,
        fn_sigs: &'a super::FnSigs,
    ) -> Self {
        let effect_entries: Vec<String> = fd
            .effects
            .iter()
            .map(|e| e.node.clone())
            .filter(|e| e != "yield")
            .collect();
        let mut lowering = Self {
            fd,
            names: Names::new(&fd.name),
            marked,
            fn_sigs,
            effect_entries,
            kind_names: HashMap::new(),
            kinds: Vec::new(),
            helpers: Vec::new(),
            errors: Vec::new(),
            tmp_counter: 0,
            stop_counter: 0,
            join_counter: 0,
        };
        lowering.name_kinds();
        lowering
    }

    // ── Diagnostics ──────────────────────────────────────────────────

    fn error(&mut self, line: usize, message: String) {
        self.errors.push(TypeError {
            message: format!("Function '{}': {message}", self.fd.name),
            line,
            col: 1,
            origin: None,
            secondary: None,
        });
    }

    fn fail<T>(&mut self, line: usize, message: String) -> Result<T, ()> {
        self.error(line, message);
        Err(())
    }

    fn internal<T>(&mut self, line: usize, what: &str) -> Result<T, ()> {
        self.fail(
            line,
            format!("internal error in yield lowering: {what}; please report this program"),
        )
    }

    // ── Stops and kinds ──────────────────────────────────────────────

    /// An operation this function declares as one of its effects. The
    /// lowering no longer cuts at these — only a marked capability is a stop
    /// — but a generated function still has to declare the ones its own
    /// segment performs in place.
    fn is_effect_name(&self, name: &str) -> bool {
        self.effect_entries.iter().any(|entry| {
            if entry == name || name.ends_with(&format!(".{entry}")) {
                return true;
            }
            // A namespace entry (`Tcp`) admits every operation of it.
            !entry.contains('.')
                && name
                    .rsplit_once('.')
                    .is_some_and(|(ns, _)| ns == entry || ns.ends_with(&format!(".{entry}")))
        })
    }

    /// The operation a stop calls, when `expr` is a stop.
    ///
    /// A stop is a call to an operation of a capability the manifest says
    /// this program answers. Nothing else is: an unmarked operation runs
    /// where it is written, inside the turn (decision 4).
    fn stop_op(&self, expr: &Spanned<Expr>) -> Option<String> {
        let Expr::FnCall(callee, _) = &expr.node else {
            return None;
        };
        let name = dotted_name(callee)?;
        self.marked.answers(&name).then_some(name)
    }

    /// Whether `expr` performs an effect where it stands: a call to an
    /// unmarked operation this function declares, or a call to a function
    /// of the program that declares effects of its own. A stop is not one —
    /// it is a request, and the lowering cuts at it.
    fn performs_effect(&self, expr: &Spanned<Expr>) -> bool {
        expr_walk::any(expr, &mut |e| {
            let Expr::FnCall(callee, _) = &e.node else {
                return false;
            };
            let Some(name) = dotted_name(callee) else {
                return false;
            };
            if self.marked.answers(&name) {
                return false;
            }
            self.is_effect_name(&name) || self.called_effects(&name).is_some_and(|e| !e.is_empty())
        })
    }

    /// The effects the program declares for a function it calls by name.
    fn called_effects(&self, name: &str) -> Option<&'a [String]> {
        self.fn_sigs
            .get(name)
            .map(|(_, _, effects)| effects.as_slice())
    }

    /// Whether `expr` holds anything the lowering has to cut at: a stop, or
    /// a `?` (which exits a segment early).
    fn needs_lowering(&self, expr: &Spanned<Expr>) -> bool {
        expr_walk::any(expr, &mut |e| {
            matches!(e.node, Expr::ErrorProp(_)) || self.stop_op(e).is_some()
        })
    }

    /// Request kinds are named after the operation (`Pool.claim` → `Claim`).
    /// Two operations of different capabilities with one short name keep
    /// their capability in the kind (`TcpRead` / `DiskRead`), and so does
    /// an operation whose own leaf name is the reserved [`YIELD_KIND`]
    /// (`Sched.yield` → `SchedYield`): the tail call owns that name.
    fn name_kinds(&mut self) {
        let mut ops: Vec<String> = Vec::new();
        for stmt in self.fd.body.stmts() {
            let expr = match stmt {
                Stmt::Binding(_, _, e) | Stmt::Expr(e) => e,
            };
            expr_walk::walk(expr, &mut |e| {
                if let Some(op) = self.stop_op(e)
                    && !ops.contains(&op)
                {
                    ops.push(op);
                }
            });
        }
        let leaf = |op: &str| capitalize(op.rsplit('.').next().unwrap_or(op));
        for op in &ops {
            let short = leaf(op);
            let ambiguous = ops.iter().any(|other| other != op && leaf(other) == short);
            let kind = if ambiguous || short == YIELD_KIND {
                let ns = op.rsplit('.').nth(1).map(capitalize).unwrap_or_default();
                let qualified = format!("{ns}{short}");
                // An operation with no capability to name it by would land
                // back on the reserved name; nothing may.
                if qualified == YIELD_KIND {
                    format!("{short}Op")
                } else {
                    qualified
                }
            } else {
                short
            };
            self.kind_names.insert(op.clone(), kind);
        }
    }

    fn kind_index(
        &mut self,
        name: &str,
        arg_types: Vec<String>,
        answer_type: Option<String>,
        line: usize,
    ) -> Result<usize, ()> {
        if let Some(index) = self.kinds.iter().position(|k| k.name == name) {
            let kind = &self.kinds[index];
            if kind.arg_types != arg_types || kind.answer_type != answer_type {
                let message = format!(
                    "two requests of kind '{name}' disagree on their argument or answer types ({} vs {})",
                    kind.arg_types.join(", "),
                    arg_types.join(", ")
                );
                return self.fail(line, message);
            }
            return Ok(index);
        }
        self.kinds.push(Kind {
            name: name.to_string(),
            arg_types,
            answer_type,
            variants: Vec::new(),
        });
        Ok(self.kinds.len() - 1)
    }

    fn variant_name(&mut self, kind: usize, bind: Option<&str>) -> String {
        self.stop_counter += 1;
        let base = match bind {
            Some(name) if !is_temp(name) && name != "_" => format!("Await{}", capitalize(name)),
            _ => format!("Await{}", self.stop_counter),
        };
        let taken =
            |kinds: &[Kind], name: &str| kinds[kind].variants.iter().any(|v| v.name == name);
        if taken(&self.kinds, &base) {
            format!("{base}{}", self.stop_counter)
        } else {
            base
        }
    }

    // ── Types from stamps ────────────────────────────────────────────

    fn type_text(&mut self, ty: Option<&Type>, line: usize, what: &str) -> Result<String, ()> {
        match ty {
            None | Some(Type::Invalid) => self.internal(line, &format!("no type recorded for {what}")),
            Some(Type::Fn(..)) => self.fail(
                line,
                format!("{what} is a function value and would be live across a request; a request state carries data only — pass what the function computes instead"),
            ),
            Some(ty) if !crate::types::checker::type_is_fully_concrete(ty) => self.internal(
                line,
                &format!("{what} has the open type '{}'", ty.display()),
            ),
            Some(ty) => Ok(ty.display()),
        }
    }

    fn stamp_text(&mut self, expr: &Spanned<Expr>, what: &str) -> Result<String, ()> {
        let ty = expr.ty().cloned();
        self.type_text(ty.as_ref(), expr.line, what)
    }

    /// Type of a live variable, read from the stamp the checker settled on
    /// for a use of it in the continuation; a parameter falls back to its
    /// declared type.
    fn live_type(
        &mut self,
        name: &str,
        rest: &[Stmt],
        tail: &Spanned<Expr>,
        ret: &Ret,
        line: usize,
    ) -> Result<String, ()> {
        let stamped = stamped_use(rest, tail, name).or_else(|| match ret {
            Ret::Inline { body } => stamped_use(&[], body, name),
            Ret::Done | Ret::Join { .. } => None,
        });
        if let Some(ty) = stamped {
            // A type the checker never settled — `{}` bound to a name
            // nothing ever fixes is `Map<K, V>` — must not be written
            // into a state variant: `K` and `V` are declared nowhere, so
            // the second check would report the generated type, the
            // generated variant and the generated constructor, three
            // complaints about names the user never wrote. One
            // diagnostic instead, at the binding, about the binding.
            if !matches!(ty, Type::Invalid | Type::Fn(..))
                && !crate::types::checker::type_is_fully_concrete(&ty)
            {
                let at = self.binding_line(name).unwrap_or(line);
                return self.fail(
                    at,
                    format!(
                        "The type of '{name}' is not settled ('{}'); a request state cannot carry an open type — give the binding a type: '{name}: <type> = ...'",
                        ty.display()
                    ),
                );
            }
            return self.type_text(Some(&ty), line, &format!("the live variable '{name}'"));
        }
        if let Some((_, ty)) = self.fd.params.iter().find(|(p, _)| p == name) {
            // A callback parameter is only ever used as a callee, which
            // carries no stamp; its declared type says what it is.
            if ty.trim_start().starts_with("Fn(") {
                return self.type_text(
                    Some(&crate::types::parse_type_str(ty)),
                    line,
                    &format!("the live variable '{name}'"),
                );
            }
            return Ok(ty.clone());
        }
        self.internal(
            line,
            &format!("no type recorded for the live variable '{name}'"),
        )
    }

    /// Where the function as written binds `name`, so a diagnostic about a
    /// variable points at the user's line for it rather than at the stop
    /// the lowering happened to reach it from. `None` for a name bound by
    /// a pattern or a parameter, which has no statement of its own.
    fn binding_line(&self, name: &str) -> Option<usize> {
        self.fd.body.stmts().iter().find_map(|stmt| match stmt {
            Stmt::Binding(bound, _, value) if bound == name => Some(value.line),
            _ => None,
        })
    }

    fn fresh_temp(&mut self) -> String {
        self.tmp_counter += 1;
        format!("__t{}", self.tmp_counter)
    }

    // ── Hoisting ─────────────────────────────────────────────────────

    /// Hoist every stop and every `?` in an unconditional position of a
    /// statement's expression into its own binding, in evaluation order,
    /// leaving the statement itself to be classified by the caller. The
    /// statement's own top-level shape (a stop, a `?`, or a `match`) is
    /// not hoisted: the caller cuts the segment at it.
    fn extract_top(
        &mut self,
        expr: &Spanned<Expr>,
        hoisted: &mut Vec<Stmt>,
    ) -> Result<Spanned<Expr>, ()> {
        // Nothing of this statement is evaluated after its own expression,
        // so the outer flag is false here; the positions inside it learn
        // from their own later siblings whether a cut follows them.
        let cut = false;
        match &expr.node {
            Expr::Match { subject, arms } => {
                let subject = self.extract(subject, hoisted, cut)?;
                Ok(spanned_like(
                    expr,
                    Expr::Match {
                        subject: Box::new(subject),
                        arms: arms.clone(),
                    },
                ))
            }
            Expr::ErrorProp(inner) => {
                let inner = self.extract(inner, hoisted, cut)?;
                Ok(spanned_like(expr, Expr::ErrorProp(Box::new(inner))))
            }
            Expr::FnCall(callee, args) if self.stop_op(expr).is_some() => {
                let args = self.extract_all(args, hoisted, cut)?;
                Ok(spanned_like(expr, Expr::FnCall(callee.clone(), args)))
            }
            Expr::TailCall(tc) => {
                let args = self.extract_all(&tc.args, hoisted, cut)?;
                Ok(spanned_like(
                    expr,
                    Expr::TailCall(Box::new(TailCallData::new(tc.target.clone(), args))),
                ))
            }
            _ => self.extract(expr, hoisted, cut),
        }
    }

    fn extract_all(
        &mut self,
        exprs: &[Spanned<Expr>],
        hoisted: &mut Vec<Stmt>,
        cut: bool,
    ) -> Result<Vec<Spanned<Expr>>, ()> {
        let positions: Vec<&Spanned<Expr>> = exprs.iter().collect();
        let flags = self.cut_flags(&positions, cut);
        let mut out = Vec::with_capacity(exprs.len());
        for (expr, cut) in exprs.iter().zip(flags) {
            out.push(self.extract(expr, hoisted, cut)?);
        }
        Ok(out)
    }

    /// For a run of positions in evaluation order, whether each one has a
    /// cut after it: a later position holding a stop or a `?`, or a cut in
    /// the expression this run sits in (`outer`). Only such a position needs
    /// its in-place effect hoisted — an effect with nothing cut after it
    /// keeps its place on its own.
    fn cut_flags(&self, positions: &[&Spanned<Expr>], outer: bool) -> Vec<bool> {
        let mut flags = vec![outer; positions.len()];
        let mut later = outer;
        for index in (0..positions.len()).rev() {
            flags[index] = later;
            later = later || self.needs_lowering(positions[index]);
        }
        flags
    }

    fn hoist(&mut self, expr: Spanned<Expr>, hoisted: &mut Vec<Stmt>) -> Spanned<Expr> {
        let temp = self.fresh_temp();
        let reference = spanned_like(&expr, Expr::Ident(temp.clone()));
        hoisted.push(Stmt::Binding(temp, None, expr));
        reference
    }

    /// Hoist stops, `?`, and branches that hold stops out of `expr`, which
    /// sits in a position that is evaluated unconditionally.
    ///
    /// `cut` says something evaluated after this expression is cut out of
    /// the statement. Such an expression is then hoisted into its own
    /// binding even when it holds no stop, as long as it performs an effect:
    /// since decision 4 a `yield` process may perform an unmarked operation
    /// in place, so siblings are no longer pure and a stop moved ahead of one
    /// would reorder two observable things. Hoisting both, in evaluation
    /// order, into the same statement list keeps the order the program wrote.
    /// An effect with nothing cut after it is left alone: the rebuilt
    /// expression follows the bindings hoisted out of it, so it already runs
    /// after them.
    fn extract(
        &mut self,
        expr: &Spanned<Expr>,
        hoisted: &mut Vec<Stmt>,
        cut: bool,
    ) -> Result<Spanned<Expr>, ()> {
        if !self.needs_lowering(expr) {
            if cut && self.performs_effect(expr) {
                return Ok(self.hoist(expr.clone(), hoisted));
            }
            return Ok(expr.clone());
        }
        let line = expr.line;
        let node = match &expr.node {
            Expr::Match { subject, arms } => {
                // The subject runs before the arms either way, and the whole
                // match is hoisted as one binding when an arm is cut, so a
                // subject that performs an effect keeps its place without
                // being hoisted on its own.
                let subject = self.extract(subject, hoisted, false)?;
                let rebuilt = spanned_like(
                    expr,
                    Expr::Match {
                        subject: Box::new(subject),
                        arms: arms.clone(),
                    },
                );
                let conditional = arms.iter().any(|arm| self.needs_lowering(&arm.body));
                return Ok(if conditional {
                    self.hoist(rebuilt, hoisted)
                } else {
                    rebuilt
                });
            }
            Expr::FnCall(callee, args) => {
                let args = self.extract_all(args, hoisted, cut)?;
                let rebuilt = spanned_like(expr, Expr::FnCall(callee.clone(), args));
                return Ok(if self.stop_op(expr).is_some() {
                    self.hoist(rebuilt, hoisted)
                } else {
                    rebuilt
                });
            }
            Expr::ErrorProp(inner) => {
                let inner = self.extract(inner, hoisted, cut)?;
                let rebuilt = spanned_like(expr, Expr::ErrorProp(Box::new(inner)));
                return Ok(self.hoist(rebuilt, hoisted));
            }
            Expr::IndependentProduct(_, unwrap) => {
                let spelling = if *unwrap { "(a, b)?!" } else { "(a, b)!" };
                return self.fail(
                    line,
                    format!(
                        "a request inside an independent product `{spelling}` is not supported by yield lowering; perform the requests one after another, or move the product into a plain function the loop calls"
                    ),
                );
            }
            Expr::TailCall(tc) => {
                let args = self.extract_all(&tc.args, hoisted, cut)?;
                Expr::TailCall(Box::new(TailCallData::new(tc.target.clone(), args)))
            }
            Expr::BinOp(op, l, r) => {
                let left_cut = cut || self.needs_lowering(r);
                Expr::BinOp(
                    *op,
                    Box::new(self.extract(l, hoisted, left_cut)?),
                    Box::new(self.extract(r, hoisted, cut)?),
                )
            }
            Expr::Neg(inner) => Expr::Neg(Box::new(self.extract(inner, hoisted, cut)?)),
            Expr::Attr(inner, field) => {
                Expr::Attr(Box::new(self.extract(inner, hoisted, cut)?), field.clone())
            }
            Expr::Constructor(name, Some(inner)) => Expr::Constructor(
                name.clone(),
                Some(Box::new(self.extract(inner, hoisted, cut)?)),
            ),
            Expr::InterpolatedStr(parts) => {
                let positions: Vec<&Spanned<Expr>> = parts
                    .iter()
                    .filter_map(|part| match part {
                        StrPart::Literal(_) => None,
                        StrPart::Parsed(inner) => Some(inner.as_ref()),
                    })
                    .collect();
                let mut flags = self.cut_flags(&positions, cut).into_iter();
                let mut out = Vec::with_capacity(parts.len());
                for part in parts {
                    out.push(match part {
                        StrPart::Literal(s) => StrPart::Literal(s.clone()),
                        StrPart::Parsed(inner) => {
                            let cut = flags.next().unwrap_or(cut);
                            StrPart::Parsed(Box::new(self.extract(inner, hoisted, cut)?))
                        }
                    });
                }
                Expr::InterpolatedStr(out)
            }
            Expr::List(items) => Expr::List(self.extract_all(items, hoisted, cut)?),
            Expr::Tuple(items) => Expr::Tuple(self.extract_all(items, hoisted, cut)?),
            Expr::MapLiteral(entries) => {
                let positions: Vec<&Spanned<Expr>> =
                    entries.iter().flat_map(|(k, v)| [k, v]).collect();
                let mut flags = self.cut_flags(&positions, cut).into_iter();
                let mut out = Vec::with_capacity(entries.len());
                for (k, v) in entries {
                    let key_cut = flags.next().unwrap_or(cut);
                    let value_cut = flags.next().unwrap_or(cut);
                    out.push((
                        self.extract(k, hoisted, key_cut)?,
                        self.extract(v, hoisted, value_cut)?,
                    ));
                }
                Expr::MapLiteral(out)
            }
            Expr::RecordCreate { type_name, fields } => {
                let positions: Vec<&Spanned<Expr>> =
                    fields.iter().map(|(_, value)| value).collect();
                let mut flags = self.cut_flags(&positions, cut).into_iter();
                let mut out = Vec::with_capacity(fields.len());
                for (name, value) in fields {
                    let cut = flags.next().unwrap_or(cut);
                    out.push((name.clone(), self.extract(value, hoisted, cut)?));
                }
                Expr::RecordCreate {
                    type_name: type_name.clone(),
                    fields: out,
                }
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                let positions: Vec<&Spanned<Expr>> = std::iter::once(base.as_ref())
                    .chain(updates.iter().map(|(_, value)| value))
                    .collect();
                let mut flags = self.cut_flags(&positions, cut).into_iter();
                let base_cut = flags.next().unwrap_or(cut);
                let base = self.extract(base, hoisted, base_cut)?;
                let mut out = Vec::with_capacity(updates.len());
                for (name, value) in updates {
                    let cut = flags.next().unwrap_or(cut);
                    out.push((name.clone(), self.extract(value, hoisted, cut)?));
                }
                Expr::RecordUpdate {
                    type_name: type_name.clone(),
                    base: Box::new(base),
                    updates: out,
                }
            }
            Expr::Literal(_)
            | Expr::Ident(_)
            | Expr::Resolved { .. }
            | Expr::Constructor(_, None) => expr.node.clone(),
        };
        Ok(spanned_like(expr, node))
    }

    // ── Segments ─────────────────────────────────────────────────────

    fn lower_block(
        &mut self,
        stmts: Vec<Stmt>,
        tail: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
    ) -> Result<Segment, ()> {
        let mut out: Vec<Stmt> = Vec::new();
        let mut pending: VecDeque<Stmt> = stmts.into();
        let mut tail = tail;
        loop {
            while let Some(stmt) = pending.pop_front() {
                let (bind, ann, expr) = match stmt {
                    Stmt::Binding(name, ann, expr) => (Some(name), ann, expr),
                    Stmt::Expr(expr) => (None, None, expr),
                };
                let mut hoisted = Vec::new();
                let expr = self.extract_top(&expr, &mut hoisted)?;
                if !hoisted.is_empty() {
                    pending.push_front(rebuild(bind, ann, expr));
                    for stmt in hoisted.into_iter().rev() {
                        pending.push_front(stmt);
                    }
                    continue;
                }
                if self.stop_op(&expr).is_some() {
                    let rest: Vec<Stmt> = pending.into();
                    let waiting = self.emit_stop(expr, bind, rest, tail, scope, ret)?;
                    return Ok(Segment {
                        stmts: out,
                        tail: waiting,
                    });
                }
                if matches!(expr.node, Expr::ErrorProp(_)) {
                    let rest: Vec<Stmt> = pending.into();
                    let branch = self.emit_prop(expr, bind, rest, tail, scope, ret)?;
                    return Ok(Segment {
                        stmts: out,
                        tail: branch,
                    });
                }
                if self.needs_lowering(&expr) {
                    if !matches!(expr.node, Expr::Match { .. }) {
                        return self.internal(expr.line, "a request survived hoisting");
                    }
                    let rest: Vec<Stmt> = pending.into();
                    let branch = self.emit_join(expr, bind, rest, tail, scope, ret)?;
                    return Ok(Segment {
                        stmts: out,
                        tail: branch,
                    });
                }
                if let Some(name) = &bind
                    && name != "_"
                {
                    scope.push(name.clone());
                }
                out.push(rebuild(bind, ann, expr));
            }
            let mut hoisted = Vec::new();
            let extracted = self.extract_top(&tail, &mut hoisted)?;
            if !hoisted.is_empty() {
                pending.extend(hoisted);
                tail = extracted;
                continue;
            }
            // A request or a `?` in tail position — the last expression of
            // the body, or the leaf of a `match` arm — is a cut like any
            // other: `extract_top` leaves the shape it sits in alone, so
            // bind it here and let the next turn of the loop cut at the
            // binding, with the rest of the path being "hand the value to
            // `ret`".
            if self.stop_op(&extracted).is_some() || matches!(extracted.node, Expr::ErrorProp(_)) {
                let temp = self.fresh_temp();
                tail = spanned_like(&extracted, Expr::Ident(temp.clone()));
                pending.push_back(Stmt::Binding(temp, None, extracted));
                continue;
            }
            let lowered = self.lower_tail(extracted, scope, ret)?;
            return Ok(Segment {
                stmts: out,
                tail: lowered,
            });
        }
    }

    /// Lower the tail of a segment, whose unconditional positions hold no
    /// stop any more: branch into the arms, turn a self tail call into a
    /// `Yield` request, and hand every other value to `ret`.
    fn lower_tail(
        &mut self,
        expr: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
    ) -> Result<Spanned<Expr>, ()> {
        match &expr.node {
            Expr::Match { subject, arms } => {
                let mut lowered = Vec::with_capacity(arms.len());
                for arm in arms {
                    let depth = scope.len();
                    pattern_binders(&arm.pattern, scope);
                    let body = self.lower_arm(arm.body.as_ref().clone(), scope, ret);
                    scope.truncate(depth);
                    lowered.push(MatchArm::new(arm.pattern.clone(), body?));
                }
                // A fresh node: the lowered match no longer has the value's
                // type, and a stale stamp would survive the second check.
                Ok(Spanned::new(
                    Expr::Match {
                        subject: subject.clone(),
                        arms: lowered,
                    },
                    expr.line,
                ))
            }
            Expr::TailCall(tc) => {
                let args = tc.args.clone();
                self.emit_yield(&tc.target, args, expr.line, ret)
            }
            Expr::FnCall(callee, args) if matches!(&callee.node, Expr::Ident(name) if name == &self.fd.name) =>
            {
                let args = args.clone();
                let target = self.fd.name.clone();
                self.emit_yield(&target, args, expr.line, ret)
            }
            _ => Ok(self.apply_ret(expr, ret)),
        }
    }

    /// A match arm body is one expression, so an arm that lowers to
    /// statements — an in-place effect hoisted out so it keeps its place
    /// before the request beside it (decision 4) — gets a function of its
    /// own to hold them, called with the variables it reads.
    fn lower_arm(
        &mut self,
        body: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
    ) -> Result<Spanned<Expr>, ()> {
        let line = body.line;
        let written = body.clone();
        let segment = self.lower_block(Vec::new(), body, scope, ret)?;
        if segment.stmts.is_empty() {
            return Ok(segment.tail);
        }
        let mut free = free_idents_of_block(&[], &written, &HashSet::new());
        free.extend(ret.live_names());
        let mut live: Vec<String> = Vec::new();
        for name in scope.iter() {
            if free.contains(name) && !live.contains(name) {
                live.push(name.clone());
            }
        }
        let mut fields = Vec::with_capacity(live.len());
        for name in &live {
            let ty = self.live_type(name, &[], &written, ret, line)?;
            fields.push((name.clone(), ty));
        }
        let (name, args) = self.join_fn(&fields, None, "", segment, line);
        Ok(call(&name, args, line))
    }

    fn apply_ret(&self, value: Spanned<Expr>, ret: &Ret) -> Spanned<Expr> {
        let line = value.line;
        match ret {
            Ret::Done => ctor(&self.names.outcome(), "Done", vec![value], line),
            Ret::Join {
                name,
                live,
                takes_value,
            } => {
                let mut args: Vec<Spanned<Expr>> = live.iter().map(|n| ident(n, line)).collect();
                if *takes_value {
                    args.push(value);
                }
                call(name, args, line)
            }
            Ret::Inline { body } => body.clone(),
        }
    }

    /// The continuation of a cut: the rest of the path, lowered, together
    /// with the variables it reads from before the cut.
    fn continuation(
        &mut self,
        bind: &Option<String>,
        rest: Vec<Stmt>,
        tail: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
        line: usize,
    ) -> Result<Continuation, ()> {
        let mut bound = HashSet::new();
        bound.extend(bind.iter().cloned());
        let mut free = free_idents_of_block(&rest, &tail, &bound);
        free.extend(ret.live_names());
        let live: Vec<String> = scope
            .iter()
            .filter(|n| free.contains(*n))
            .cloned()
            .collect();
        let mut fields = Vec::with_capacity(live.len());
        for name in &live {
            let ty = self.live_type(name, &rest, &tail, ret, line)?;
            fields.push((name.clone(), ty));
        }
        let uses_bind = bind.as_ref().is_some_and(|b| mentions(&rest, &tail, b));
        let depth = scope.len();
        scope.extend(bind.iter().cloned());
        let segment = self.lower_block(rest, tail, scope, ret);
        scope.truncate(depth);
        Ok(Continuation {
            fields,
            segment: segment?,
            uses_bind,
        })
    }

    fn waiting(
        &self,
        kind: &str,
        mut args: Vec<Spanned<Expr>>,
        state: Spanned<Expr>,
        line: usize,
    ) -> Spanned<Expr> {
        args.push(state);
        let request = ctor(&self.names.request(), kind, args, line);
        ctor(&self.names.outcome(), "Waiting", vec![request], line)
    }

    fn emit_stop(
        &mut self,
        call_expr: Spanned<Expr>,
        bind: Option<String>,
        rest: Vec<Stmt>,
        tail: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
    ) -> Result<Spanned<Expr>, ()> {
        let line = call_expr.line;
        let Expr::FnCall(callee, args) = &call_expr.node else {
            return self.internal(line, "a stop that is not a call");
        };
        let op = dotted_name(callee).unwrap_or_default();
        let kind_name = self
            .kind_names
            .get(&op)
            .cloned()
            .unwrap_or_else(|| capitalize(op.rsplit('.').next().unwrap_or(&op)));
        let answer_type = self.stamp_text(&call_expr, &format!("the answer of '{op}'"))?;
        let mut arg_types = Vec::with_capacity(args.len());
        for (index, arg) in args.iter().enumerate() {
            arg_types.push(self.stamp_text(arg, &format!("argument {index} of '{op}'"))?);
        }
        let kind = self.kind_index(&kind_name, arg_types, Some(answer_type.clone()), line)?;
        let variant = self.variant_name(kind, bind.as_deref());
        // `Unit` carries nothing: the answer function takes the state only,
        // and a binding of the answer reads as the `Unit` value itself.
        let answer: Spanned<Expr> = if answer_type == "Unit" {
            Spanned::new(Expr::Literal(Literal::Unit), line)
        } else {
            ident("__answer", line)
        };
        // Reserve the variant's place before the continuation registers
        // the stops after it, so two stops of the same kind on one
        // straight-line path number in source order. That guarantee does
        // not reach across a join: `emit_join` lowers the arms' shared
        // tail before the arms themselves (the tail's join function has
        // to exist, by name, for an arm to call it), so a stop in the
        // code after a `match` can claim an earlier number than a stop
        // inside one of the match's own arms, even though the arm's stop
        // reads first in the source.
        let slot = self.kinds[kind].variants.len();
        self.kinds[kind].variants.push(Variant {
            name: variant.clone(),
            fields: Vec::new(),
            arm: ident("__answer", line),
        });
        let Continuation {
            fields,
            mut segment,
            uses_bind,
        } = self.continuation(&bind, rest, tail, scope, ret, line)?;

        // The rest reads the answer under the user's name; the answer
        // function has it as `__answer` (a `Unit` answer as the value).
        // Substituting keeps the user's spans and needs no binder — an
        // irrefutable pattern would be one, and the backends do not lower
        // a bare binder over every subject type alike.
        if let (Some(name), true) = (&bind, uses_bind) {
            substitute_free_in_block(&mut segment.stmts, &mut segment.tail, name, &answer);
        }
        let arm = if segment.stmts.is_empty() {
            segment.tail
        } else {
            let name = self.names.after(&variant);
            let mut params = fields.clone();
            let mut call_args: Vec<Spanned<Expr>> =
                fields.iter().map(|(n, _)| ident(n, line)).collect();
            // The answer is not part of the state — it arrives with the
            // answer function — so it is a parameter of the continuation
            // and not a field of the variant, and it is passed only when
            // the continuation still reads it after the substitution above.
            if free_idents_of_block(&segment.stmts, &segment.tail, &HashSet::new())
                .contains("__answer")
            {
                params.push(("__answer".to_string(), answer_type.clone()));
                call_args.push(ident("__answer", line));
            }
            self.helpers.push(fn_def(
                name.clone(),
                params,
                self.names.outcome(),
                Some(format!(
                    "Continues '{}' after the answer to its '{op}' request at line {line}.",
                    self.fd.name
                )),
                segment.stmts,
                segment.tail,
                line,
            ));
            call(&name, call_args, line)
        };

        let state_args: Vec<Spanned<Expr>> = fields.iter().map(|(n, _)| ident(n, line)).collect();
        let state = ctor(&self.names.state(&kind_name), &variant, state_args, line);
        self.kinds[kind].variants[slot] = Variant {
            name: variant,
            fields,
            arm,
        };
        Ok(self.waiting(&kind_name, args.clone(), state, line))
    }

    /// `x = e?` cuts the path in two: `Err` leaves the function through
    /// `Done`, `Ok` continues with `x` bound.
    fn emit_prop(
        &mut self,
        prop: Spanned<Expr>,
        bind: Option<String>,
        rest: Vec<Stmt>,
        tail: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
    ) -> Result<Spanned<Expr>, ()> {
        let line = prop.line;
        let Expr::ErrorProp(inner) = &prop.node else {
            return self.internal(line, "a `?` that is not a `?`");
        };
        let ok_type = self.stamp_text(&prop, "the value of a `?`")?;
        let Continuation {
            fields,
            segment,
            uses_bind,
        } = self.continuation(&bind, rest, tail, scope, ret, line)?;
        let ok_binder = match (&bind, uses_bind) {
            (Some(name), true) => name.clone(),
            _ => "_".to_string(),
        };
        let ok_body = if segment.stmts.is_empty() {
            segment.tail
        } else {
            let (name, args) =
                self.join_fn(&fields, bind.filter(|_| uses_bind), &ok_type, segment, line);
            call(&name, args, line)
        };
        let err_body = ctor(
            &self.names.outcome(),
            "Done",
            vec![ctor("Result", "Err", vec![ident("__err", line)], line)],
            line,
        );
        Ok(match_expr(
            inner.as_ref().clone(),
            vec![
                MatchArm::new(
                    Pattern::Constructor("Result.Err".to_string(), vec!["__err".to_string()]),
                    err_body,
                ),
                MatchArm::new(
                    Pattern::Constructor("Result.Ok".to_string(), vec![ok_binder]),
                    ok_body,
                ),
            ],
            line,
        ))
    }

    /// A statement-level `match` with a request in an arm: the rest of the
    /// path becomes the arms' shared continuation.
    fn emit_join(
        &mut self,
        branch: Spanned<Expr>,
        bind: Option<String>,
        rest: Vec<Stmt>,
        tail: Spanned<Expr>,
        scope: &mut Vec<String>,
        ret: &Ret,
    ) -> Result<Spanned<Expr>, ()> {
        let line = branch.line;
        let Expr::Match { subject, arms } = &branch.node else {
            return self.internal(line, "a join that is not a match");
        };
        let value_type = match &bind {
            Some(_) => Some(self.stamp_text(&branch, "the value of a match")?),
            None => None,
        };
        let Continuation {
            fields,
            segment,
            uses_bind,
        } = self.continuation(&bind, rest, tail, scope, ret, line)?;
        // The rest follows in place only when it does not read the value:
        // a bound value needs a binder, and the one binder an expression
        // offers — an irrefutable pattern — is not lowered over every
        // subject type by every backend. A continuation function binds it
        // as a parameter instead.
        let inner_ret = if segment.stmts.is_empty() && !uses_bind {
            Ret::Inline { body: segment.tail }
        } else {
            let value_type = value_type.unwrap_or_default();
            let (name, _) = self.join_fn(
                &fields,
                bind.filter(|_| uses_bind),
                &value_type,
                segment,
                line,
            );
            Ret::Join {
                name,
                live: fields.iter().map(|(n, _)| n.clone()).collect(),
                takes_value: uses_bind,
            }
        };
        let mut lowered = Vec::with_capacity(arms.len());
        for arm in arms {
            let depth = scope.len();
            pattern_binders(&arm.pattern, scope);
            let body = self.lower_arm(arm.body.as_ref().clone(), scope, &inner_ret);
            scope.truncate(depth);
            lowered.push(MatchArm::new(arm.pattern.clone(), body?));
        }
        Ok(Spanned::new(
            Expr::Match {
                subject: subject.clone(),
                arms: lowered,
            },
            line,
        ))
    }

    /// A generated continuation function over the live variables, plus the
    /// bound value when the rest reads it. Returns its name and the
    /// argument list a caller passes from the cut point (the value last).
    fn join_fn(
        &mut self,
        fields: &[(String, String)],
        value: Option<String>,
        value_type: &str,
        segment: Segment,
        line: usize,
    ) -> (String, Vec<Spanned<Expr>>) {
        self.join_counter += 1;
        let name = self.names.join(self.join_counter);
        let mut params = fields.to_vec();
        let mut args: Vec<Spanned<Expr>> = fields.iter().map(|(n, _)| ident(n, line)).collect();
        if let Some(value) = value {
            args.push(ident(&value, line));
            params.push((value, value_type.to_string()));
        }
        self.helpers.push(fn_def(
            name.clone(),
            params,
            self.names.outcome(),
            Some(format!(
                "Continues '{}' after the branch at line {line}.",
                self.fd.name
            )),
            segment.stmts,
            segment.tail,
            line,
        ));
        (name, args)
    }

    /// A self tail call is a `Yield` request whose state is the argument
    /// tuple; answering it re-enters `Start`.
    fn emit_yield(
        &mut self,
        target: &str,
        args: Vec<Spanned<Expr>>,
        line: usize,
        ret: &Ret,
    ) -> Result<Spanned<Expr>, ()> {
        if target != self.fd.name {
            return self.fail(
                line,
                format!(
                    "tail-calls '{target}', which is not lowered as a Yield request in this phase; only a self tail call is"
                ),
            );
        }
        if !matches!(ret, Ret::Done) {
            return self.internal(line, "a tail call outside tail position");
        }
        if args.len() != self.fd.params.len() {
            return self.internal(line, "a self tail call with the wrong arity");
        }
        let kind = self.kind_index(YIELD_KIND, Vec::new(), None, line)?;
        let variant = self.variant_name(kind, None);
        let fields: Vec<(String, String)> = self.fd.params.clone();
        let resume_args: Vec<Spanned<Expr>> = fields.iter().map(|(n, _)| ident(n, line)).collect();
        let arm = call(&self.names.start(), resume_args, line);
        let state = ctor(&self.names.state(YIELD_KIND), &variant, args, line);
        self.kinds[kind].variants.push(Variant {
            name: variant,
            fields,
            arm,
        });
        Ok(self.waiting(YIELD_KIND, Vec::new(), state, line))
    }

    // ── Assembly ─────────────────────────────────────────────────────

    fn run(&mut self) -> Result<Generated, ()> {
        let fd = self.fd;
        let stmts = fd.body.stmts().to_vec();
        let Some(Stmt::Expr(_)) = stmts.last() else {
            return self.fail(fd.line, "the body must end with an expression".to_string());
        };
        let mut stmts = stmts;
        let Some(Stmt::Expr(tail)) = stmts.pop() else {
            unreachable!("checked above");
        };
        let mut scope: Vec<String> = fd.params.iter().map(|(n, _)| n.clone()).collect();
        let start = self.lower_block(stmts, tail, &mut scope, &Ret::Done)?;
        if self.kinds.is_empty() {
            return self.fail(
                fd.line,
                "declares `yield` but never stops: it calls no operation of a capability this program answers, and it does not tail-call itself. Mark the capability it should wait on — `answer = \"<Module>\"` on its `[[providers.bindings]]` entry in aver.toml — or remove `yield` from its effect list".to_string(),
            );
        }

        let line = fd.line;
        let names = &self.names;
        let mut items: Vec<TopLevel> = Vec::new();
        let mut public_names: Vec<String> = Vec::new();

        for kind in &self.kinds {
            let state = names.state(&kind.name);
            let variants = kind
                .variants
                .iter()
                .map(|v| {
                    (
                        v.name.clone(),
                        v.fields.iter().map(|(_, t)| t.clone()).collect(),
                    )
                })
                .collect();
            items.push(TopLevel::TypeDef(sum_type(state.clone(), variants, line)));
            public_names.push(state);
        }
        let request_variants = self
            .kinds
            .iter()
            .map(|kind| {
                let mut fields = kind.arg_types.clone();
                fields.push(names.state(&kind.name));
                (kind.name.clone(), fields)
            })
            .collect();
        items.push(TopLevel::TypeDef(sum_type(
            names.request(),
            request_variants,
            line,
        )));
        items.push(TopLevel::TypeDef(sum_type(
            names.outcome(),
            vec![
                ("Done".to_string(), vec![fd.return_type.clone()]),
                ("Waiting".to_string(), vec![names.request()]),
            ],
            line,
        )));
        public_names.push(names.request());
        public_names.push(names.outcome());

        items.push(TopLevel::FnDef(fn_def(
            names.start(),
            fd.params.clone(),
            names.outcome(),
            fd.desc.clone(),
            start.stmts,
            start.tail,
            line,
        )));
        public_names.push(names.start());

        for kind in &self.kinds {
            let state = names.state(&kind.name);
            let arms = kind
                .variants
                .iter()
                .map(|v| {
                    MatchArm::new(
                        Pattern::Constructor(
                            format!("{state}.{}", v.name),
                            v.fields.iter().map(|(n, _)| n.clone()).collect(),
                        ),
                        v.arm.clone(),
                    )
                })
                .collect();
            let mut params = vec![("__state".to_string(), state)];
            let desc = match &kind.answer_type {
                Some(answer) => {
                    if answer != "Unit" {
                        params.push(("__answer".to_string(), answer.clone()));
                    }
                    format!(
                        "Resumes '{}' after the coordinator answered its {} request.",
                        fd.name, kind.name
                    )
                }
                None => format!(
                    "Re-enters '{}' at its tail call with the arguments the Yield request carries.",
                    fd.name
                ),
            };
            let name = names.answer(&kind.name);
            items.push(TopLevel::FnDef(fn_def(
                name.clone(),
                params,
                names.outcome(),
                Some(desc),
                Vec::new(),
                match_expr(ident("__state", line), arms, line),
                line,
            )));
            public_names.push(name);
        }
        items.extend(self.helpers.drain(..).map(TopLevel::FnDef));
        self.assign_effects(&mut items);
        let protocol = super::ProcessProtocol {
            fn_name: fd.name.clone(),
            params: fd.params.clone(),
            return_type: fd.return_type.clone(),
            start: names.start(),
            request: names.request(),
            outcome: names.outcome(),
            kinds: self
                .kinds
                .iter()
                .map(|kind| super::ProtocolKind {
                    name: kind.name.clone(),
                    operation: self
                        .kind_names
                        .iter()
                        .find(|(_, named)| *named == &kind.name)
                        .map(|(operation, _)| operation.clone()),
                    arg_types: kind.arg_types.clone(),
                    answer_type: kind.answer_type.clone(),
                    state: names.state(&kind.name),
                    answer_fn: names.answer(&kind.name),
                    variants: kind
                        .variants
                        .iter()
                        .map(|variant| (variant.name.clone(), variant.fields.len()))
                        .collect(),
                })
                .collect(),
        };
        Ok(Generated {
            public_names,
            items,
            protocol,
        })
    }
}

impl Lowering<'_> {
    /// Decision 4: a generated function carries exactly the unmarked
    /// operations its own segment performs, computed here and declared by
    /// nobody. A process that calls no unmarked operation keeps every
    /// generated function pure, as they all were before this leg.
    ///
    /// A segment that calls another generated function — a continuation
    /// helper, or `Start` from an answered `Yield` — performs what that one
    /// performs, so the direct sets are closed over the generated call graph
    /// before they are written down.
    fn assign_effects(&self, items: &mut [TopLevel]) {
        let positions: Vec<usize> = items
            .iter()
            .enumerate()
            .filter(|(_, item)| matches!(item, TopLevel::FnDef(_)))
            .map(|(pos, _)| pos)
            .collect();
        let index: HashMap<String, usize> = positions
            .iter()
            .enumerate()
            .filter_map(|(slot, pos)| match &items[*pos] {
                TopLevel::FnDef(fd) => Some((fd.name.clone(), slot)),
                _ => None,
            })
            .collect();
        let mut direct: Vec<std::collections::BTreeSet<String>> = Vec::new();
        let mut calls: Vec<Vec<usize>> = Vec::new();
        for pos in &positions {
            let TopLevel::FnDef(fd) = &items[*pos] else {
                continue;
            };
            let mut own = std::collections::BTreeSet::new();
            let mut called = Vec::new();
            for stmt in fd.body.stmts() {
                let expr = match stmt {
                    Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr,
                };
                expr_walk::walk(expr, &mut |e| {
                    let name = match &e.node {
                        Expr::FnCall(callee, _) => dotted_name(callee),
                        Expr::TailCall(tc) => Some(tc.target.clone()),
                        _ => None,
                    };
                    let Some(name) = name else { return };
                    if let Some(slot) = index.get(&name) {
                        called.push(*slot);
                        return;
                    }
                    if self.marked.answers(&name) {
                        return;
                    }
                    if self.is_effect_name(&name) {
                        own.insert(name);
                        return;
                    }
                    if let Some(effects) = self.called_effects(&name) {
                        // A callee that declares a marked operation is a
                        // plain function performing a request, which
                        // `intercept-outside-yield` refuses by name. Its
                        // effects are carried here as they are declared:
                        // dropping the marked one would leave the generated
                        // function calling an effect it does not declare, and
                        // a second error about a generated name is not a
                        // better report of the same mistake.
                        own.extend(
                            effects
                                .iter()
                                .filter(|effect| *effect != super::YIELD_EFFECT)
                                .cloned(),
                        );
                    }
                });
            }
            direct.push(own);
            calls.push(called);
        }
        loop {
            let mut changed = false;
            for slot in 0..direct.len() {
                let grown: Vec<String> = calls[slot]
                    .iter()
                    .flat_map(|callee| direct[*callee].iter().cloned())
                    .collect();
                for effect in grown {
                    changed |= direct[slot].insert(effect);
                }
            }
            if !changed {
                break;
            }
        }
        for (slot, pos) in positions.iter().enumerate() {
            let TopLevel::FnDef(fd) = &mut items[*pos] else {
                continue;
            };
            let line = fd.line;
            fd.effects = direct[slot]
                .iter()
                .map(|effect| Spanned::new(effect.clone(), line))
                .collect();
        }
    }
}

fn is_temp(name: &str) -> bool {
    name.starts_with("__t")
}

fn rebuild(bind: Option<String>, ann: Option<String>, expr: Spanned<Expr>) -> Stmt {
    match bind {
        Some(name) => Stmt::Binding(name, ann, expr),
        None => Stmt::Expr(expr),
    }
}
