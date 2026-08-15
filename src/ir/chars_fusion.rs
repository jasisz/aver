//! Chars fusion — walk a string without building the list of its
//! characters, and decide a character by its codepoint.
//!
//! Two rewrites, one pass, because they only pay off together: the
//! canonical character loop hands each character to a helper that
//! matches it against single-character literals, so removing the list
//! and removing the string comparisons are the two halves of the same
//! program.
//!
//! **Cursor.** `String.chars(s)` builds one one-character string per
//! character AND a cons cell per character, and the loop that consumes
//! it immediately takes them apart again. When the list is handed
//! straight into a self-recursive function that only ever destructures
//! it, the whole list is dead weight:
//!
//! ```aver
//! fn digits(chars: List<String>, acc: Int) -> Int
//!     match chars
//!         [] -> acc
//!         [head, ..tail] -> digits(tail, acc + value(head))
//!
//! fn count(text: String) -> Int
//!     digits(String.chars(text), 0)
//! ```
//!
//! becomes a walk over `text` itself — a `(String, Int)` cursor pair in
//! place of the list parameter, and a synthesized `digits__cursor` that
//! reads the character at the cursor instead of a cons cell:
//!
//! ```aver
//! fn digits__cursor(__cur_s: String, __cur_i: Int, acc: Int) -> Int
//!     match __str_cursor_end(__cur_s, __cur_i)
//!         true -> acc
//!         false -> match __str_cursor_head(__cur_s, __cur_i)
//!             head -> match __str_cursor_next(__cur_s, __cur_i)
//!                 __cur_i1 -> digits__cursor(__cur_s, __cur_i1, acc + value(head))
//! ```
//!
//! The original function is left exactly as it was; only call sites
//! that hand it a fresh `String.chars(…)` move to the variant.
//!
//! **Codepoint match.** A match whose arms are single-character string
//! literals compares strings once per arm. Reading the subject's
//! codepoint once and matching integers is the same decision:
//!
//! ```aver
//! match String.toLower(digit)
//!     "0" -> Option.Some(0)
//!     …
//!     _ -> Option.None
//! ```
//!
//! becomes `match __str_code1_lower(digit) { 48 -> …; _ -> … }`.
//! `__str_code1*` answers `-1` for any string that is not exactly one
//! character, which is what makes the rewrite exact rather than
//! approximate: `-1` equals none of the rewritten literals, so the
//! default arm fires for precisely the strings that matched no literal
//! before. The folding forms call the same `to_lowercase` /
//! `to_uppercase` the builtin calls, so `U+212A KELVIN SIGN` — whose
//! lowercase is the ASCII `"k"` — keeps the arm it always took, and
//! `U+0130`, which lowercases to two characters, keeps the default arm
//! it always took.
//!
//! ## Where this sits
//!
//! Below the proof line (see [`crate::ir::pipeline::AstView`]): the
//! pass fabricates entities the source does not contain — a
//! `<fn>__cursor` function and six `__str_*` intrinsics — so a theorem
//! about its output would be a theorem about a program nobody wrote.
//! `pipeline::run` snapshots the AST before it, which makes that
//! structural rather than a convention.
//!
//! It runs on the AST rather than on MIR for a measured reason: on the
//! program this was built for, MIR emits none of the functions involved
//! (`Call(Builtin)` blocks the lowering), so an MIR pass would be
//! invisible in exactly the place it has to work.
//!
//! ## What it declines
//!
//! Everything it has not proven. The failure mode that matters is a
//! wrong fuse, not a missed one, so the recogniser is a single
//! traversal that both checks and rewrites — there is no second pass
//! that could disagree with the first — and every tracked list name is
//! occurs-checked: it may be read at the two places the rewrite has a
//! replacement for (the subject of a `[] / [h, ..t]` match, and the
//! list argument of the self-call) and nowhere else. A binder that
//! shadows a tracked name takes it out of scope for that subtree, so
//! reads after the shadow are reads of the shadow. Anything else —
//! `List.len(chars)`, handing the list to another function, a third
//! arm, a nested pattern the rewrite has no cursor form for — declines
//! the whole loop and says so through `--explain-passes`.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::Arc;

use crate::ast::{
    Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart, TailCallData, TopLevel,
};

/// Prefix for every name this pass invents inside a function body. A
/// loop that already uses one — reads it OR binds it — declines rather
/// than risk capturing it.
///
/// A leading `__` is not reserved: `__cur_i = 3` parses, type-checks and
/// runs, so this is a shape a program can reach and not a guard against
/// other passes. Binding matters more than reading, and in the more
/// dangerous direction: a binder spelled `__cur_i` shadows the cursor
/// parameter for everything underneath it, so the emitted end test and
/// step would read the user's number instead of the offset — an answer
/// off the end of the string, or an infinite loop when the number is
/// inside it.
const CURSOR_PREFIX: &str = "__cur_";

/// The synthesized cursor variant's string parameter.
const CURSOR_STR_PARAM: &str = "__cur_s";

/// The synthesized cursor variant's byte-offset parameter.
const CURSOR_IDX_PARAM: &str = "__cur_i";

/// Suffix distinguishing the synthesized variant from the loop it was
/// built from.
const CURSOR_SUFFIX: &str = "__cursor";

/// Suffix of the classifier variant that takes the character's
/// codepoint instead of its one-character string.
const CODE_SUFFIX: &str = "__code";

/// The classifier variant's one parameter. Chosen once and checked
/// against the callee's body: a body that reads or binds this name
/// could capture it, so such a callee simply grows no variant.
const CODE_PARAM: &str = "code";

/// Prefix of the fresh binder that holds the head's codepoint in a
/// rewritten peel. Under `CURSOR_PREFIX`, so a loop body that could
/// collide was already declined when its cursor variant was built, and
/// the per-variant counter keeps the pass's own binders apart — nested
/// peels each bind their own code, and a shared name would hand an
/// outer classifier the INNER character's code.
const CODE_BIND_PREFIX: &str = "__cur_c";

/// The builtin namespace this pass reads `chars` / `toLower` /
/// `toUpper` off. A local binding cannot shadow it: `String.x(…)`
/// resolves to the builtin whatever else carries the name, so there is
/// nothing here to check against the enclosing scope.
const STRING_NAMESPACE: &str = "String";

/// Prefix of every intrinsic this pass emits CALLS to —
/// `__str_cursor_end` / `__str_cursor_head` / `__str_cursor_next` /
/// `__str_cursor_code`, the `__str_code1*` classifiers and the
/// `__str_fold_*` folds. Same reasoning as [`CURSOR_PREFIX`], from the
/// other side of the call: the calls are emitted as bare identifiers
/// and resolved later, so a user binder carrying one of these names
/// anywhere in the function captures the emitted call — the resolver
/// reads the callee as the local, not the intrinsic. A function that
/// binds into this namespace is stepped around, not rewritten.
const INTRINSIC_PREFIX: &str = "__str_";

/// Why the cursor recogniser turned a loop down. Carried into the pass
/// report so `--explain-passes` can say which loop stayed on the list
/// and what about it stopped the rewrite — a silent decline is how a
/// fusion quietly stops firing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharsFusionDecline {
    /// The parameter fed by `String.chars` is not a plain `List<String>`
    /// binding.
    ParamShape,
    /// `String.chars(…)` reaches two different parameters of the same
    /// function; a cursor variant carries one cursor, so it cannot
    /// stand in for both lists.
    ///
    /// Conservatism rather than safety: fusing whichever list came
    /// first would leave the other materialised and still answer
    /// correctly, because a call site is only rewritten when the
    /// producer sits in the position the variant was built for. The
    /// rule is here because "one cursor variant, one list" is what this
    /// rewrite can say it does, and picking a winner by argument order
    /// is not. Two cursors is a separate shape, and it can have its own
    /// recogniser when a workload asks for it.
    ConflictingProducers,
    /// A `<fn>__cursor` name is already taken, or the body already uses
    /// the `__cur_` namespace, or the function binds a name in the
    /// `__str_` namespace the emitted intrinsic calls live in.
    NameTaken,
    /// The tracked list is read somewhere the rewrite has no cursor
    /// form for — a length, a pass to another function, a return.
    ListEscapes,
    /// The match over the tracked list is not the `[] / [head, ..tail]`
    /// pair the cursor replaces.
    MatchShape,
    /// The self-call does not pass a tracked list in the list position.
    RecursiveCallShape,
    /// A binder between a head binding and one of its reads shadows a
    /// name the codepoint-call rewrite would have to read there — the
    /// cursor string, the binding's offset, or the fresh code binder.
    /// Unreachable through the shapes this pass itself emits (its own
    /// binders are fresh, and a body spelling `__cur_` was declined
    /// before its variant was built), so this is the belt-and-braces
    /// answer should either invariant ever loosen.
    CodeBindShadowed,
}

impl CharsFusionDecline {
    /// One-line explanation, rendered by `--explain-passes`.
    pub const fn reason(self) -> &'static str {
        match self {
            Self::ParamShape => "the parameter fed by String.chars is not a plain List<String>",
            Self::ConflictingProducers => {
                "String.chars is handed to two different parameters of this function"
            }
            Self::NameTaken => {
                "the __cursor variant name or the __cur_ or __str_ namespace is already taken"
            }
            Self::ListEscapes => "the list is read somewhere a cursor cannot stand in for",
            Self::MatchShape => "the match over the list is not the [] / [head, ..tail] pair",
            Self::RecursiveCallShape => "the recursive call does not step the list it was given",
            Self::CodeBindShadowed => {
                "a binder between the head binding and a read shadows a cursor name"
            }
        }
    }
}

/// What the pass did. Every field is a fact a CI gate can assert on
/// without parsing prose.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CharsFusionPassReport {
    /// Producer call sites rewritten from `f(String.chars(x), …)` to
    /// `f__cursor(x, 0, …)`.
    pub cursor_rewrites: usize,
    /// Names of the synthesized `<fn>__cursor` variants appended to the
    /// items list.
    pub synthesized: Vec<String>,
    /// Loop fns whose cursor variant fired (`synthesized` minus the
    /// suffix). Sorted.
    pub loop_fns: Vec<String>,
    /// Matches over single-character string literals rewritten to a
    /// codepoint comparison.
    pub codepoint_matches: usize,
    /// Classifier calls inside synthesized cursor variants rewritten
    /// from `f(<head>)` to `f__code(<code>)` — the head's codepoint
    /// crosses the call boundary instead of a one-character string.
    pub codepoint_calls: usize,
    /// Per-fn codepoint-match counts, alphabetised.
    pub codepoint_matches_by_fn: BTreeMap<String, usize>,
    /// Loops the cursor recogniser looked at and turned down, with the
    /// reason. Alphabetised by fn name.
    pub declined: BTreeMap<String, &'static str>,
}

impl CharsFusionPassReport {
    /// Did anything at all fire? Used by the diagnostic renderers to
    /// pick between "nothing detected" and the itemised report.
    pub fn fired(&self) -> bool {
        self.cursor_rewrites > 0 || self.codepoint_matches > 0
    }
}

/// Run the full chars-fusion pass on a program's items.
///
/// Must run AFTER `tco::transform_program` (the recursive call it steps
/// is usually an `Expr::TailCall` by then) and BEFORE
/// `resolver::resolve_program` (everything here matches on
/// `Expr::Ident`, which the resolver rewrites to `Expr::Resolved`).
pub fn run_chars_fusion_pass(items: &mut Vec<TopLevel>) -> CharsFusionPassReport {
    let mut report = CharsFusionPassReport::default();

    let fn_names: HashSet<String> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd.name.clone()),
            _ => None,
        })
        .collect();
    let taken = taken_names(items);

    // Which functions are handed a fresh `String.chars(…)`, and in
    // which parameter position. Collected first so the shape check runs
    // once per function rather than once per call site.
    let mut producers: BTreeMap<String, BTreeSet<usize>> = BTreeMap::new();
    for fd in fn_defs(items) {
        let scope = CallScope::of(fd, &fn_names);
        for stmt in fd.body.stmts() {
            collect_producer_sites(stmt_expr(stmt), &scope, &mut producers);
        }
    }

    let mut accepted: HashMap<String, AcceptedLoop> = HashMap::new();
    let mut variants: Vec<FnDef> = Vec::new();
    for (name, positions) in &producers {
        let Some(fd) = fn_defs(items).find(|fd| &fd.name == name) else {
            continue;
        };
        let new_name = format!("{name}{CURSOR_SUFFIX}");
        let outcome = if positions.len() != 1 {
            Err(CharsFusionDecline::ConflictingProducers)
        } else if taken.contains(&new_name) {
            Err(CharsFusionDecline::NameTaken)
        } else {
            let list_idx = *positions.iter().next().expect("exactly one position");
            build_cursor_variant(fd, list_idx, &new_name).map(|variant| (list_idx, variant))
        };
        match outcome {
            Ok((list_idx, variant)) => {
                accepted.insert(
                    name.clone(),
                    AcceptedLoop {
                        list_idx,
                        new_name: new_name.clone(),
                    },
                );
                variants.push(variant);
            }
            Err(decline) => {
                report.declined.insert(name.clone(), decline.reason());
            }
        }
    }

    // Rewrite the call sites that produced the candidates. A variant
    // whose call sites all turn out unrewriteable is dropped rather
    // than left in the program as dead synthesized code.
    let mut fired: HashSet<String> = HashSet::new();
    for fd in fn_defs_mut(items) {
        let scope = CallScope::of(fd, &fn_names);
        let body = Arc::make_mut(&mut fd.body);
        for stmt in body.stmts_mut() {
            report.cursor_rewrites +=
                rewrite_producer_calls(stmt_expr_mut(stmt), &scope, &accepted, &mut fired);
        }
    }
    variants.retain(|v| fired.contains(&v.name));
    report.synthesized = variants.iter().map(|v| v.name.clone()).collect();
    report.synthesized.sort();
    report.loop_fns = report
        .synthesized
        .iter()
        .map(|n| {
            n.strip_suffix(CURSOR_SUFFIX)
                .expect("every synthesized name carries the suffix")
                .to_string()
        })
        .collect();
    items.reserve(variants.len());
    for variant in variants {
        items.push(TopLevel::FnDef(variant));
    }

    // The codepoint rewrite runs last so the synthesized variants —
    // which carry the same match sites the originals do — are covered
    // by the same walk.
    for fd in fn_defs_mut(items) {
        // This rewrite emits bare `__str_code1*` calls into whatever
        // function the match sits in — cursor loop or not. A function
        // that binds a name in that namespace would hand the emitted
        // call to its own binder, so it keeps its string matches.
        let mut bound = HashSet::new();
        collect_bound_names(fd, &mut bound);
        if bound.iter().any(|n| n.starts_with(INTRINSIC_PREFIX)) {
            continue;
        }
        let mut count = 0;
        let body = Arc::make_mut(&mut fd.body);
        for stmt in body.stmts_mut() {
            count += rewrite_codepoint_matches(stmt_expr_mut(stmt));
        }
        if count > 0 {
            *report
                .codepoint_matches_by_fn
                .entry(fd.name.clone())
                .or_default() += count;
            report.codepoint_matches += count;
        }
    }

    // And after it, the call boundary: a classifier the codepoint
    // rewrite just reduced to one `__str_code1*` match can take the
    // code itself, so a cursor loop that only ever hands its head to
    // such classifiers binds the head's codepoint instead of the
    // one-character string.
    rewrite_codepoint_calls(items, &taken, &mut report);

    report
}

/// Is there anything here for the pass to look at? Read-only, so a
/// caller that has to decide whether to COPY a module before running
/// the pass can ask first — the VM re-parses every dependency and has
/// to re-run the pass on its own copy, and most modules hold neither
/// shape.
///
/// Deliberately loose: a `true` costs one wasted traversal, a `false`
/// costs the rewrite. It answers for both halves of the pass, because a
/// module with only single-character matches and no character loop is
/// still a module this pass speeds up.
pub fn has_fusable_shape(items: &[TopLevel]) -> bool {
    let fn_names: HashSet<String> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd.name.clone()),
            _ => None,
        })
        .collect();
    let mut producers = BTreeMap::new();
    let mut codepoint_match = false;
    for fd in fn_defs(items) {
        let scope = CallScope::of(fd, &fn_names);
        for stmt in fd.body.stmts() {
            collect_producer_sites(stmt_expr(stmt), &scope, &mut producers);
            codepoint_match = codepoint_match || has_single_char_match(stmt_expr(stmt));
        }
    }
    !producers.is_empty() || codepoint_match
}

fn has_single_char_match(expr: &Spanned<Expr>) -> bool {
    if let Expr::Match { arms, .. } = &expr.node
        && single_char_arm_codes(arms).is_some()
    {
        return true;
    }
    let mut found = false;
    walk_children(expr, &mut |child| {
        found = found || has_single_char_match(child);
    });
    found
}

/// A loop the recogniser accepted: which parameter carried the list,
/// and what the variant standing in for it is called.
struct AcceptedLoop {
    list_idx: usize,
    new_name: String,
}

// ── Producer detection ──────────────────────────────────────────────

/// Which top-level function names one body may call by bare name.
///
/// A name is a top-level function only if the body does not bind it
/// first: `fn go(walk: Fn(List<String>, Int) -> Int, text: String)`
/// calling `walk(String.chars(text), 0)` is calling its own parameter,
/// and rewriting that call to `walk__cursor` would call a different
/// function than the program does.
///
/// Deliberately coarse: a binder anywhere in the body hides the name for
/// the WHOLE body, not just its own scope. Costing a fusion we could
/// have kept is the safe direction, and the shape that would need the
/// finer answer — a local shadowing a same-named top-level function in
/// one branch only — has never appeared.
struct CallScope<'a> {
    callable: HashSet<&'a str>,
}

impl<'a> CallScope<'a> {
    fn of(fd: &FnDef, fn_names: &'a HashSet<String>) -> Self {
        let mut bound = HashSet::new();
        collect_bound_names(fd, &mut bound);
        Self {
            callable: fn_names
                .iter()
                .filter(|name| !bound.contains(*name))
                .map(String::as_str)
                .collect(),
        }
    }
}

/// Record every `f(…, String.chars(x), …)` where `f` is a function in
/// this module, keyed by `f` with the argument positions the producer
/// appeared in.
fn collect_producer_sites(
    expr: &Spanned<Expr>,
    scope: &CallScope<'_>,
    out: &mut BTreeMap<String, BTreeSet<usize>>,
) {
    if let Expr::FnCall(callee, args) = &expr.node
        && let Expr::Ident(name) = &callee.node
        && scope.callable.contains(name.as_str())
    {
        for (i, arg) in args.iter().enumerate() {
            if is_string_chars_call(&arg.node) {
                out.entry(name.clone()).or_default().insert(i);
            }
        }
    }
    walk_children(expr, &mut |child| collect_producer_sites(child, scope, out));
}

/// `String.chars(<one arg>)`.
fn is_string_chars_call(expr: &Expr) -> bool {
    matches!(expr, Expr::FnCall(callee, args)
        if args.len() == 1 && is_dotted(&callee.node, STRING_NAMESPACE, "chars"))
}

/// `<Module>.<member>` — the uncalled callee shape of a builtin call.
fn is_dotted(expr: &Expr, module: &str, member: &str) -> bool {
    matches!(expr, Expr::Attr(base, attr)
        if attr == member && matches!(&base.node, Expr::Ident(n) if n == module))
}

/// Rewrite `f(…, String.chars(x), …)` into `f__cursor(…, x, 0, …)` for
/// every accepted loop. Returns how many call sites moved.
fn rewrite_producer_calls(
    expr: &mut Spanned<Expr>,
    scope: &CallScope<'_>,
    accepted: &HashMap<String, AcceptedLoop>,
    fired: &mut HashSet<String>,
) -> usize {
    let mut count = 0;
    walk_children_mut(expr, &mut |child| {
        count += rewrite_producer_calls(child, scope, accepted, fired);
    });

    let line = expr.line;
    let Expr::FnCall(callee, args) = &mut expr.node else {
        return count;
    };
    let Expr::Ident(name) = &callee.node else {
        return count;
    };
    if !scope.callable.contains(name.as_str()) {
        return count;
    }
    let Some(loop_info) = accepted.get(name) else {
        return count;
    };
    let Some(producer) = args.get(loop_info.list_idx) else {
        return count;
    };
    let Expr::FnCall(_, producer_args) = &producer.node else {
        return count;
    };
    if !is_string_chars_call(&producer.node) {
        return count;
    }
    let subject = producer_args[0].clone();
    let mut new_args: Vec<Spanned<Expr>> = Vec::with_capacity(args.len() + 1);
    new_args.extend(args[..loop_info.list_idx].iter().cloned());
    new_args.push(subject);
    new_args.push(Spanned::new(Expr::Literal(Literal::Int(0)), line));
    new_args.extend(args[loop_info.list_idx + 1..].iter().cloned());
    expr.node = Expr::FnCall(
        Box::new(Spanned::new(Expr::Ident(loop_info.new_name.clone()), line)),
        new_args,
    );
    fired.insert(loop_info.new_name.clone());
    count + 1
}

// ── Cursor variant synthesis ────────────────────────────────────────

/// Names tracked as cursors, innermost binding last. A `Vec` rather
/// than a map because the depth is the nesting depth of cons patterns —
/// two or three in practice — and because a shadowing binder has to be
/// able to hide an outer entry without losing it for the sibling
/// scopes.
type Cursors = Vec<(String, String)>;

fn cursor_of<'a>(cursors: &'a Cursors, name: &str) -> Option<&'a str> {
    cursors
        .iter()
        .rev()
        .find(|(n, _)| n == name)
        .map(|(_, idx)| idx.as_str())
}

/// Drop every tracking of `name` — the caller is about to introduce a
/// binder that shadows it, and reads inside that scope are reads of the
/// binder, not of the list.
fn shadow(cursors: &Cursors, name: &str) -> Cursors {
    cursors.iter().filter(|(n, _)| n != name).cloned().collect()
}

/// Build the `<fn>__cursor` variant, or say what stopped it.
///
/// Check and rewrite are the same traversal on purpose: a separate
/// "is this shape safe?" walk is a second implementation of the same
/// judgement, and the two drifting apart is precisely how a fusion
/// starts answering a different question than the loop it replaced.
fn build_cursor_variant(
    fd: &FnDef,
    list_idx: usize,
    new_name: &str,
) -> Result<FnDef, CharsFusionDecline> {
    let (list_name, list_ty) = fd
        .params
        .get(list_idx)
        .ok_or(CharsFusionDecline::ParamShape)?;
    if !is_list_of_string(list_ty) || list_name == "_" {
        return Err(CharsFusionDecline::ParamShape);
    }
    // A body that already spells `__cur_` could have one of the
    // generated binders captured out from under it — or, the other way
    // round, could shadow one of them and send the cursor somewhere the
    // rewrite never put it. Reads and binders are asked separately
    // because they are different traversals, not because one implies the
    // other. And a body that binds its OWN name makes the self-call
    // recognition below a lie.
    //
    // A binder in the `__str_` namespace is the same capture from the
    // call side: the variant's peels call `__str_cursor_*` by bare
    // name, and a parameter, statement binding or pattern binder
    // spelled like one of them is what those calls would resolve to.
    // One whole-function answer on purpose — a bound name anywhere in
    // the function reaches every position a later stage could put the
    // emitted call in, the re-materialised cold read under a
    // between-bind-and-read binder included.
    let mut bound = HashSet::new();
    collect_bound_names(fd, &mut bound);
    if bound.iter().any(|n| n.starts_with(CURSOR_PREFIX))
        || bound.iter().any(|n| n.starts_with(INTRINSIC_PREFIX))
        || fd
            .body
            .stmts()
            .iter()
            .any(|s| mentions_prefix(stmt_expr(s), CURSOR_PREFIX))
        || bound.contains(&fd.name)
    {
        return Err(CharsFusionDecline::NameTaken);
    }

    let mut cx = Cursorer {
        self_name: &fd.name,
        new_name,
        list_idx,
        fresh: 0,
    };
    let mut cursors: Cursors = vec![(list_name.clone(), CURSOR_IDX_PARAM.to_string())];

    let mut new_stmts = Vec::with_capacity(fd.body.stmts().len());
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Expr(e) => new_stmts.push(Stmt::Expr(cx.transform(e, &cursors)?)),
            Stmt::Binding(name, ty, e) => {
                let lowered = cx.transform(e, &cursors)?;
                cursors = shadow(&cursors, name);
                new_stmts.push(Stmt::Binding(name.clone(), ty.clone(), lowered));
            }
        }
    }

    let mut params: Vec<(String, String)> = fd.params[..list_idx].to_vec();
    params.push((CURSOR_STR_PARAM.to_string(), "String".to_string()));
    params.push((CURSOR_IDX_PARAM.to_string(), "Int".to_string()));
    params.extend(fd.params[list_idx + 1..].iter().cloned());

    Ok(FnDef {
        name: new_name.to_string(),
        line: fd.line,
        params,
        return_type: fd.return_type.clone(),
        // The variant runs the same body at the same positions, so it
        // has the same effect surface. Conservative and exact at once.
        effects: fd.effects.clone(),
        desc: Some(format!(
            "Synthesized cursor variant of `{}`. Call sites that hand it \
             `String.chars(s)` walk `s` directly — a byte offset stepped \
             one codepoint at a time — instead of materialising the list \
             of one-character strings.",
            fd.name
        )),
        body: Arc::new(FnBody::Block(new_stmts)),
        resolution: None,
    })
}

/// The traversal that turns a list loop into a cursor loop.
struct Cursorer<'a> {
    self_name: &'a str,
    new_name: &'a str,
    list_idx: usize,
    fresh: usize,
}

/// `__str_<name>(__cur_s, <idx>)`.
fn cursor_call(name: &str, idx: &str, line: crate::ast::SourceLine) -> Spanned<Expr> {
    sp(
        Expr::FnCall(
            Box::new(sp(Expr::Ident(name.to_string()), line)),
            vec![
                sp(Expr::Ident(CURSOR_STR_PARAM.to_string()), line),
                sp(Expr::Ident(idx.to_string()), line),
            ],
        ),
        line,
    )
}

/// A single-arm match used as a binding — Aver has no expression `let`,
/// and duplicating the head read at every use of the binder would
/// recompute it (and, for a loop that reads its character twice,
/// allocate twice).
fn bind(
    name: &str,
    value: Spanned<Expr>,
    body: Spanned<Expr>,
    line: crate::ast::SourceLine,
) -> Spanned<Expr> {
    let pattern = if name == "_" {
        Pattern::Wildcard
    } else {
        Pattern::Ident(name.to_string())
    };
    sp(
        Expr::Match {
            subject: Box::new(value),
            arms: vec![MatchArm {
                pattern,
                body: Box::new(body),
                binding_slots: std::sync::OnceLock::new(),
            }],
        },
        line,
    )
}

fn sp(node: Expr, line: crate::ast::SourceLine) -> Spanned<Expr> {
    Spanned::new(node, line)
}

impl Cursorer<'_> {
    fn next_index_name(&mut self) -> String {
        self.fresh += 1;
        format!("{CURSOR_IDX_PARAM}{}", self.fresh)
    }

    fn transform(
        &mut self,
        expr: &Spanned<Expr>,
        cursors: &Cursors,
    ) -> Result<Spanned<Expr>, CharsFusionDecline> {
        let line = expr.line;
        match &expr.node {
            // A bare read of a tracked list. Every position the rewrite
            // has a replacement for is handled by the parent before it
            // recurses, so reaching here means the list is being used
            // as a list — and the variant has none.
            Expr::Ident(n) | Expr::Resolved { name: n, .. } if cursor_of(cursors, n).is_some() => {
                Err(CharsFusionDecline::ListEscapes)
            }

            Expr::Match { subject, arms } => {
                if let Expr::Ident(n) = &subject.node
                    && let Some(idx) = cursor_of(cursors, n).map(str::to_string)
                {
                    return self.transform_list_match(&idx, arms, cursors, line);
                }
                // Not a tracked subject — but the subject expression is
                // still scanned, because `match List.len(chars) { … }`
                // reads the list just as surely as returning it does.
                let new_subject = self.transform(subject, cursors)?;
                let mut new_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    let inner = pattern_bindings(&arm.pattern)
                        .iter()
                        .fold(cursors.clone(), |acc, name| shadow(&acc, name));
                    new_arms.push(MatchArm {
                        pattern: arm.pattern.clone(),
                        body: Box::new(self.transform(&arm.body, &inner)?),
                        binding_slots: std::sync::OnceLock::new(),
                    });
                }
                Ok(sp(
                    Expr::Match {
                        subject: Box::new(new_subject),
                        arms: new_arms,
                    },
                    line,
                ))
            }

            Expr::FnCall(callee, args) => {
                if let Expr::Ident(name) = &callee.node
                    && name == self.self_name
                {
                    let new_args = self.transform_self_call_args(args, cursors, line)?;
                    return Ok(sp(
                        Expr::FnCall(
                            Box::new(sp(Expr::Ident(self.new_name.to_string()), line)),
                            new_args,
                        ),
                        line,
                    ));
                }
                let new_callee = self.transform(callee, cursors)?;
                let new_args = self.transform_each(args, cursors)?;
                Ok(sp(Expr::FnCall(Box::new(new_callee), new_args), line))
            }

            Expr::TailCall(data) if data.target == self.self_name => {
                let new_args = self.transform_self_call_args(&data.args, cursors, line)?;
                Ok(sp(
                    Expr::TailCall(Box::new(TailCallData::new(
                        self.new_name.to_string(),
                        new_args,
                    ))),
                    line,
                ))
            }

            // Everything else is structural. The match is exhaustive on
            // purpose: a variant nobody classified is a read nobody
            // counted, and an uncounted read is a wrong fuse.
            Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => Ok(expr.clone()),
            Expr::Attr(base, field) => Ok(sp(
                Expr::Attr(Box::new(self.transform(base, cursors)?), field.clone()),
                line,
            )),
            Expr::BinOp(op, lhs, rhs) => Ok(sp(
                Expr::BinOp(
                    *op,
                    Box::new(self.transform(lhs, cursors)?),
                    Box::new(self.transform(rhs, cursors)?),
                ),
                line,
            )),
            Expr::Neg(inner) => Ok(sp(
                Expr::Neg(Box::new(self.transform(inner, cursors)?)),
                line,
            )),
            Expr::ErrorProp(inner) => Ok(sp(
                Expr::ErrorProp(Box::new(self.transform(inner, cursors)?)),
                line,
            )),
            Expr::Constructor(name, payload) => {
                let lowered = match payload {
                    Some(p) => Some(Box::new(self.transform(p, cursors)?)),
                    None => None,
                };
                Ok(sp(Expr::Constructor(name.clone(), lowered), line))
            }
            Expr::InterpolatedStr(parts) => {
                let mut out = Vec::with_capacity(parts.len());
                for part in parts {
                    out.push(match part {
                        StrPart::Literal(s) => StrPart::Literal(s.clone()),
                        StrPart::Parsed(e) => {
                            StrPart::Parsed(Box::new(self.transform(e, cursors)?))
                        }
                    });
                }
                Ok(sp(Expr::InterpolatedStr(out), line))
            }
            Expr::List(items) => Ok(sp(Expr::List(self.transform_each(items, cursors)?), line)),
            Expr::Tuple(items) => Ok(sp(Expr::Tuple(self.transform_each(items, cursors)?), line)),
            Expr::IndependentProduct(items, unwrap) => Ok(sp(
                Expr::IndependentProduct(self.transform_each(items, cursors)?, *unwrap),
                line,
            )),
            Expr::MapLiteral(entries) => {
                let mut out = Vec::with_capacity(entries.len());
                for (k, v) in entries {
                    out.push((self.transform(k, cursors)?, self.transform(v, cursors)?));
                }
                Ok(sp(Expr::MapLiteral(out), line))
            }
            Expr::RecordCreate { type_name, fields } => {
                let mut out = Vec::with_capacity(fields.len());
                for (name, value) in fields {
                    out.push((name.clone(), self.transform(value, cursors)?));
                }
                Ok(sp(
                    Expr::RecordCreate {
                        type_name: type_name.clone(),
                        fields: out,
                    },
                    line,
                ))
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                let new_base = self.transform(base, cursors)?;
                let mut out = Vec::with_capacity(updates.len());
                for (name, value) in updates {
                    out.push((name.clone(), self.transform(value, cursors)?));
                }
                Ok(sp(
                    Expr::RecordUpdate {
                        type_name: type_name.clone(),
                        base: Box::new(new_base),
                        updates: out,
                    },
                    line,
                ))
            }
            Expr::TailCall(data) => {
                let new_args = self.transform_each(&data.args, cursors)?;
                Ok(sp(
                    Expr::TailCall(Box::new(TailCallData::new(data.target.clone(), new_args))),
                    line,
                ))
            }
        }
    }

    fn transform_each(
        &mut self,
        exprs: &[Spanned<Expr>],
        cursors: &Cursors,
    ) -> Result<Vec<Spanned<Expr>>, CharsFusionDecline> {
        exprs.iter().map(|e| self.transform(e, cursors)).collect()
    }

    /// `match <tracked list> { [] -> nil; [head, ..tail] -> cons }` →
    /// an end test, a head binding, and a stepped cursor for `tail`.
    fn transform_list_match(
        &mut self,
        idx: &str,
        arms: &[MatchArm],
        cursors: &Cursors,
        line: crate::ast::SourceLine,
    ) -> Result<Spanned<Expr>, CharsFusionDecline> {
        if arms.len() != 2 {
            return Err(CharsFusionDecline::MatchShape);
        }
        let mut nil_body: Option<&Spanned<Expr>> = None;
        let mut cons: Option<(&String, &String, &Spanned<Expr>)> = None;
        for arm in arms {
            match &arm.pattern {
                Pattern::EmptyList if nil_body.is_none() => nil_body = Some(&arm.body),
                Pattern::Cons(head, tail) if cons.is_none() => {
                    cons = Some((head, tail, &arm.body));
                }
                _ => return Err(CharsFusionDecline::MatchShape),
            }
        }
        let (Some(nil_body), Some((head, tail, cons_body))) = (nil_body, cons) else {
            return Err(CharsFusionDecline::MatchShape);
        };
        // `[x, ..x]` binds the same name twice; which one a later read
        // means is not something this rewrite should be deciding.
        if head == tail && head != "_" {
            return Err(CharsFusionDecline::MatchShape);
        }

        let nil_arm = self.transform(nil_body, cursors)?;

        let next_idx = self.next_index_name();
        let mut inner_cursors = shadow(cursors, head);
        inner_cursors = shadow(&inner_cursors, tail);
        if tail != "_" {
            inner_cursors.push((tail.clone(), next_idx.clone()));
        }
        let cons_arm = self.transform(cons_body, &inner_cursors)?;

        let stepped = bind(
            &next_idx,
            cursor_call("__str_cursor_next", idx, line),
            cons_arm,
            line,
        );
        let with_head = bind(
            head,
            cursor_call("__str_cursor_head", idx, line),
            stepped,
            line,
        );

        Ok(sp(
            Expr::Match {
                subject: Box::new(cursor_call("__str_cursor_end", idx, line)),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(nil_arm),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(false)),
                        body: Box::new(with_head),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            },
            line,
        ))
    }

    /// Arguments of the self-call: the list position must be a tracked
    /// list (which becomes the string plus its stepped offset), and no
    /// other argument may mention one.
    fn transform_self_call_args(
        &mut self,
        args: &[Spanned<Expr>],
        cursors: &Cursors,
        line: crate::ast::SourceLine,
    ) -> Result<Vec<Spanned<Expr>>, CharsFusionDecline> {
        let Some(list_arg) = args.get(self.list_idx) else {
            return Err(CharsFusionDecline::RecursiveCallShape);
        };
        let Expr::Ident(name) = &list_arg.node else {
            return Err(CharsFusionDecline::RecursiveCallShape);
        };
        let Some(idx) = cursor_of(cursors, name) else {
            return Err(CharsFusionDecline::RecursiveCallShape);
        };
        let idx = idx.to_string();

        let mut out: Vec<Spanned<Expr>> = Vec::with_capacity(args.len() + 1);
        for arg in &args[..self.list_idx] {
            out.push(self.transform(arg, cursors)?);
        }
        out.push(sp(Expr::Ident(CURSOR_STR_PARAM.to_string()), line));
        out.push(sp(Expr::Ident(idx), line));
        for arg in &args[self.list_idx + 1..] {
            out.push(self.transform(arg, cursors)?);
        }
        Ok(out)
    }
}

// ── Codepoint match rewrite ─────────────────────────────────────────

/// Rewrite every `match` whose arms are single-character ASCII string
/// literals into a match on the subject's codepoint. Returns how many
/// fired.
fn rewrite_codepoint_matches(expr: &mut Spanned<Expr>) -> usize {
    let mut count = 0;
    walk_children_mut(expr, &mut |child| {
        count += rewrite_codepoint_matches(child);
    });

    let line = expr.line;
    let Expr::Match { subject, arms } = &mut expr.node else {
        return count;
    };
    let Some(codes) = single_char_arm_codes(arms) else {
        return count;
    };
    // Folding the case into the intrinsic is the only part that
    // assumes anything about the subject; `__str_code1(E)` is exact for
    // whatever `E` turns out to be.
    let (intrinsic, inner) = match &subject.node {
        Expr::FnCall(callee, args)
            if args.len() == 1 && is_dotted(&callee.node, STRING_NAMESPACE, "toLower") =>
        {
            ("__str_code1_lower", args[0].clone())
        }
        Expr::FnCall(callee, args)
            if args.len() == 1 && is_dotted(&callee.node, STRING_NAMESPACE, "toUpper") =>
        {
            ("__str_code1_upper", args[0].clone())
        }
        _ => ("__str_code1", (**subject).clone()),
    };

    let new_subject = Spanned::new(
        Expr::FnCall(
            Box::new(Spanned::new(Expr::Ident(intrinsic.to_string()), line)),
            vec![inner],
        ),
        line,
    );
    for (arm, code) in arms.iter_mut().zip(codes) {
        if let Some(code) = code {
            arm.pattern = Pattern::Literal(Literal::Int(code));
        }
    }
    **subject = new_subject;
    count + 1
}

/// The codepoint of each arm's literal, or `None` for the single
/// default arm. `None` for the whole match when it is not the shape the
/// rewrite is exact on: at least one single-character ASCII literal,
/// every other arm either such a literal or the one trailing default,
/// and the default binding nothing — a binder would come out holding
/// the codepoint instead of the string it was written to hold.
fn single_char_arm_codes(arms: &[MatchArm]) -> Option<Vec<Option<i64>>> {
    if arms.len() < 2 {
        return None;
    }
    let mut codes = Vec::with_capacity(arms.len());
    let mut literals = 0usize;
    for (i, arm) in arms.iter().enumerate() {
        match &arm.pattern {
            Pattern::Literal(Literal::Str(s)) => {
                let mut chars = s.chars();
                match (chars.next(), chars.next()) {
                    (Some(c), None) if c.is_ascii() => {
                        literals += 1;
                        codes.push(Some(i64::from(u32::from(c))));
                    }
                    _ => return None,
                }
            }
            Pattern::Wildcard | Pattern::Ident(_) if i + 1 == arms.len() => {
                if let Pattern::Ident(name) = &arm.pattern
                    && name != "_"
                {
                    return None;
                }
                codes.push(None);
            }
            _ => return None,
        }
    }
    (literals > 0 && codes.last().is_some_and(Option::is_none)).then_some(codes)
}

// ── Codepoint across the call boundary ──────────────────────────────
//
// The cursor stage binds each character as a one-character string so a
// String-typed classifier can consume it — which is exactly one arena
// string per character on the VM and one `AverStr` per character in
// compiled Rust, twice over in a loop that peels pairs. When the
// classifier's whole body is the `match __str_code1*(param)` the
// codepoint rewrite just produced, the string is a detour: the
// classifier can take the code. This stage synthesizes that variant
// (`<fn>__code(code: Int)`) and rebinds the peel to
// `__str_cursor_code` wherever every read of the head either crosses
// into such a classifier or can re-materialise the head on the spot.
// The String classifier stays in the program for every other caller.

/// Which case fold a qualifying classifier's subject applies — the
/// `__str_code1` family member it matched on, and therefore the
/// codepoint-level fold its variant's subject needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodeFold {
    Bare,
    Lower,
    Upper,
}

/// Detect qualifying classifiers, rewrite the peels of this run's
/// cursor variants to bind the codepoint, and append a `<fn>__code`
/// variant for every classifier a rewrite actually reached.
fn rewrite_codepoint_calls(
    items: &mut Vec<TopLevel>,
    taken: &HashSet<String>,
    report: &mut CharsFusionPassReport,
) {
    // The peel shape this stage rewrites exists only inside cursor
    // variants synthesized by this same run.
    if report.synthesized.is_empty() {
        return;
    }
    // Opportunity detection, not judgement: a callee that almost
    // qualifies is a classifier this stage cannot serve yet, and says
    // nothing — the loop it sits in keeps the head binding it has.
    let mut callees: BTreeMap<String, CodeFold> = BTreeMap::new();
    for fd in fn_defs(items) {
        if let Some(fold) = qualifies_as_code_callee(fd)
            && !taken.contains(&format!("{}{CODE_SUFFIX}", fd.name))
        {
            callees.insert(fd.name.clone(), fold);
        }
    }
    if callees.is_empty() {
        return;
    }

    let callee_names: HashSet<String> = callees.keys().cloned().collect();
    let cursor_names: HashSet<String> = report.synthesized.iter().cloned().collect();
    let mut used: HashSet<String> = HashSet::new();
    for fd in fn_defs_mut(items) {
        if !cursor_names.contains(&fd.name) {
            continue;
        }
        match rewrite_code_binds(fd, &callee_names, &mut used) {
            Ok(calls) => report.codepoint_calls += calls,
            Err(decline) => {
                let loop_name = fd
                    .name
                    .strip_suffix(CURSOR_SUFFIX)
                    .unwrap_or(&fd.name)
                    .to_string();
                report.declined.insert(loop_name, decline.reason());
            }
        }
    }
    if used.is_empty() {
        return;
    }

    // Variants only for the classifiers a rewrite reached — the same
    // rule that drops a cursor variant whose call sites never moved.
    let mut variants: Vec<FnDef> = Vec::new();
    for (name, fold) in &callees {
        let variant_name = format!("{name}{CODE_SUFFIX}");
        if !used.contains(&variant_name) {
            continue;
        }
        let fd = fn_defs(items)
            .find(|fd| &fd.name == name)
            .expect("collected from these items above");
        variants.push(build_code_variant(fd, *fold, &variant_name));
        report.synthesized.push(variant_name);
    }
    report.synthesized.sort();
    items.reserve(variants.len());
    for variant in variants {
        items.push(TopLevel::FnDef(variant));
    }
}

/// Is this the classifier shape whose parameter can cross as a code?
///
/// Everything is required, or no variant: one `String` parameter, no
/// effects, a body that is EXACTLY the `match __str_code1*(param)` with
/// integer-literal arms and a trailing bare wildcard (the shape the
/// codepoint-match rewrite just produced), no self-mention, and the
/// parameter read nowhere but the subject — a read anywhere else would
/// be left with no string to read. The variant's own parameter name
/// must also be absent from the body, or an arm could capture it.
fn qualifies_as_code_callee(fd: &FnDef) -> Option<CodeFold> {
    let [(param, ty)] = fd.params.as_slice() else {
        return None;
    };
    if ty.replace(char::is_whitespace, "") != "String" || param == "_" {
        return None;
    }
    if !fd.effects.is_empty() {
        return None;
    }
    let [stmt] = fd.body.stmts() else {
        return None;
    };
    let Stmt::Expr(expr) = stmt else {
        return None;
    };
    let Expr::Match { subject, arms } = &expr.node else {
        return None;
    };
    let Expr::FnCall(callee, args) = &subject.node else {
        return None;
    };
    let Expr::Ident(intrinsic) = &callee.node else {
        return None;
    };
    let fold = match intrinsic.as_str() {
        "__str_code1" => CodeFold::Bare,
        "__str_code1_lower" => CodeFold::Lower,
        "__str_code1_upper" => CodeFold::Upper,
        _ => return None,
    };
    if args.len() != 1 || !matches!(&args[0].node, Expr::Ident(n) if n == param) {
        return None;
    }
    if arms.len() < 2 {
        return None;
    }
    let (default, literals) = arms.split_last().expect("two arms or more");
    if !matches!(default.pattern, Pattern::Wildcard)
        || !literals
            .iter()
            .all(|arm| matches!(arm.pattern, Pattern::Literal(Literal::Int(_))))
    {
        return None;
    }
    for arm in arms {
        if mentions_name(&arm.body, param)
            || mentions_name(&arm.body, CODE_PARAM)
            || mentions_name(&arm.body, &fd.name)
        {
            return None;
        }
    }
    let mut bound = HashSet::new();
    collect_bound_names(fd, &mut bound);
    bound.remove(param);
    if bound.contains(CODE_PARAM) {
        return None;
    }
    Some(fold)
}

/// Build `<fn>__code(code: Int)` from a qualifying classifier: the same
/// arms over a subject that folds the codepoint instead of decoding a
/// one-character string.
fn build_code_variant(fd: &FnDef, fold: CodeFold, new_name: &str) -> FnDef {
    let [Stmt::Expr(expr)] = fd.body.stmts() else {
        unreachable!("qualified as exactly one expression statement")
    };
    let Expr::Match { subject, arms } = &expr.node else {
        unreachable!("qualified as exactly one match")
    };
    let line = subject.line;
    let code_read = sp(Expr::Ident(CODE_PARAM.to_string()), line);
    let new_subject = match fold {
        CodeFold::Bare => code_read,
        CodeFold::Lower | CodeFold::Upper => {
            let intrinsic = if fold == CodeFold::Lower {
                "__str_fold_lower"
            } else {
                "__str_fold_upper"
            };
            sp(
                Expr::FnCall(
                    Box::new(sp(Expr::Ident(intrinsic.to_string()), line)),
                    vec![code_read],
                ),
                line,
            )
        }
    };
    let new_match = sp(
        Expr::Match {
            subject: Box::new(new_subject),
            arms: arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: arm.body.clone(),
                    binding_slots: std::sync::OnceLock::new(),
                })
                .collect(),
        },
        expr.line,
    );
    FnDef {
        name: new_name.to_string(),
        line: fd.line,
        params: vec![(CODE_PARAM.to_string(), "Int".to_string())],
        return_type: fd.return_type.clone(),
        effects: fd.effects.clone(),
        desc: Some(format!(
            "Synthesized codepoint variant of `{}`. A cursor loop whose \
             head only ever reaches this classifier hands over the \
             character's code instead of materialising a one-character \
             string.",
            fd.name
        )),
        body: Arc::new(FnBody::Block(vec![Stmt::Expr(new_match)])),
        resolution: None,
    }
}

/// Rewrite the peels of one cursor variant. `Ok(n)` with the number of
/// classifier calls moved onto the code; `Err` — and an untouched body —
/// when a shadow makes a read unanswerable, which the pass's own fresh
/// names make unreachable today (see
/// [`CharsFusionDecline::CodeBindShadowed`]).
fn rewrite_code_binds(
    fd: &mut FnDef,
    callees: &HashSet<String>,
    used: &mut HashSet<String>,
) -> Result<usize, CharsFusionDecline> {
    let mut bound = HashSet::new();
    collect_bound_names(fd, &mut bound);
    let mut cx = CodeCaller {
        callees,
        bound,
        fresh: 0,
        used_local: HashSet::new(),
        calls: 0,
    };
    // Work on a copy, so declining leaves the variant exactly as the
    // cursor stage emitted it.
    let mut stmts: Vec<Stmt> = fd.body.stmts().to_vec();
    for stmt in &mut stmts {
        cx.transform(stmt_expr_mut(stmt))?;
    }
    if cx.calls > 0 {
        fd.body = Arc::new(FnBody::Block(stmts));
        used.extend(cx.used_local);
    }
    Ok(cx.calls)
}

/// The traversal that rebinds a peel from the head to its codepoint.
struct CodeCaller<'a> {
    callees: &'a HashSet<String>,
    /// Every name the variant's body binds anywhere. Coarse on purpose,
    /// like `CallScope`: a classifier name bound ANYWHERE in this body
    /// makes every bare call of it suspect, and the cost of the coarse
    /// answer is one head that stays materialised.
    bound: HashSet<String>,
    fresh: usize,
    /// `<fn>__code` names the committed rewrites call.
    used_local: HashSet<String>,
    /// Classifier calls moved onto the code.
    calls: usize,
}

/// The offset a peel's head binding reads, when `subject` is exactly
/// the emitted `__str_cursor_head(__cur_s, <idx>)`.
fn head_bind_offset(subject: &Spanned<Expr>) -> Option<String> {
    let Expr::FnCall(callee, args) = &subject.node else {
        return None;
    };
    if !matches!(&callee.node, Expr::Ident(n) if n == "__str_cursor_head") || args.len() != 2 {
        return None;
    }
    let (Expr::Ident(s), Expr::Ident(i)) = (&args[0].node, &args[1].node) else {
        return None;
    };
    (s == CURSOR_STR_PARAM && i.starts_with(CURSOR_IDX_PARAM)).then(|| i.clone())
}

impl CodeCaller<'_> {
    /// The name of the qualifying classifier this call hands the head
    /// to, alone — or `None` for every other expression.
    fn hot_callee(&self, expr: &Expr, h: &str) -> Option<String> {
        let Expr::FnCall(callee, args) = expr else {
            return None;
        };
        let Expr::Ident(f) = &callee.node else {
            return None;
        };
        if !self.callees.contains(f) || self.bound.contains(f) || args.len() != 1 {
            return None;
        }
        matches!(&args[0].node, Expr::Ident(x) if x == h).then(|| f.clone())
    }

    /// Bottom-up over the variant body: inner peels commit first, so an
    /// outer peel's read analysis sees exactly the reads that survive.
    fn transform(&mut self, expr: &mut Spanned<Expr>) -> Result<(), CharsFusionDecline> {
        let mut result: Result<(), CharsFusionDecline> = Ok(());
        walk_children_mut(expr, &mut |child| {
            if result.is_ok() {
                result = self.transform(child);
            }
        });
        result?;

        let line = expr.line;
        let Expr::Match { subject, arms } = &mut expr.node else {
            return Ok(());
        };
        if arms.len() != 1 {
            return Ok(());
        }
        let Some(idx) = head_bind_offset(subject) else {
            return Ok(());
        };
        let Pattern::Ident(h) = arms[0].pattern.clone() else {
            return Ok(());
        };
        // Fresh per peel — nested peels each bind their own code, and a
        // shared name would hand an outer classifier the INNER
        // character's code.
        self.fresh += 1;
        let code_name = format!("{CODE_BIND_PREFIX}{}", self.fresh);

        // Analyze and rewrite one clone of the arm body; commit only
        // when at least one read actually crossed a call boundary —
        // otherwise binding the code buys nothing and the head binding
        // stays.
        let mut body = (*arms[0].body).clone();
        let mut hot = 0usize;
        let mut shadows: Vec<String> = Vec::new();
        let mut pending: HashSet<String> = HashSet::new();
        self.rewrite_reads(
            &mut body,
            &h,
            &idx,
            &code_name,
            &mut shadows,
            &mut hot,
            &mut pending,
        )?;
        if hot == 0 {
            return Ok(());
        }
        **subject = cursor_call("__str_cursor_code", &idx, line);
        arms[0].pattern = Pattern::Ident(code_name);
        *arms[0].body = body;
        self.calls += hot;
        self.used_local.extend(pending);
        Ok(())
    }

    /// Every read of `h` under one peel: the sole argument of a
    /// qualifying classifier moves onto the code; anything else
    /// re-materialises the head at `idx` — the offset the BINDING read,
    /// which later peels' stepped offsets do not change.
    #[allow(clippy::too_many_arguments)]
    fn rewrite_reads(
        &mut self,
        expr: &mut Spanned<Expr>,
        h: &str,
        idx: &str,
        code_name: &str,
        shadows: &mut Vec<String>,
        hot: &mut usize,
        pending: &mut HashSet<String>,
    ) -> Result<(), CharsFusionDecline> {
        let line = expr.line;
        if let Some(f) = self.hot_callee(&expr.node, h) {
            if shadows.iter().any(|n| n == code_name) {
                return Err(CharsFusionDecline::CodeBindShadowed);
            }
            let Expr::FnCall(callee, args) = &mut expr.node else {
                unreachable!("hot_callee only answers for calls")
            };
            let variant = format!("{f}{CODE_SUFFIX}");
            callee.node = Expr::Ident(variant.clone());
            args[0].node = Expr::Ident(code_name.to_string());
            pending.insert(variant);
            *hot += 1;
            return Ok(());
        }
        if matches!(&expr.node, Expr::Ident(x) if x == h) {
            if shadows.iter().any(|n| n == CURSOR_STR_PARAM || n == idx) {
                return Err(CharsFusionDecline::CodeBindShadowed);
            }
            *expr = cursor_call("__str_cursor_head", idx, line);
            return Ok(());
        }
        if let Expr::Match { subject, arms } = &mut expr.node {
            self.rewrite_reads(subject, h, idx, code_name, shadows, hot, pending)?;
            for arm in arms {
                let binders = pattern_bindings(&arm.pattern);
                // Reads under a binder that reuses the head's name are
                // reads of that binder, not of the head.
                if binders.iter().any(|b| b == h) {
                    continue;
                }
                let depth = shadows.len();
                shadows.extend(binders);
                let walked =
                    self.rewrite_reads(&mut arm.body, h, idx, code_name, shadows, hot, pending);
                shadows.truncate(depth);
                walked?;
            }
            return Ok(());
        }
        let mut result: Result<(), CharsFusionDecline> = Ok(());
        walk_children_mut(expr, &mut |child| {
            if result.is_ok() {
                result = self.rewrite_reads(child, h, idx, code_name, shadows, hot, pending);
            }
        });
        result
    }
}

/// Does any identifier read or tail-call target under `expr` carry
/// exactly this name? A tail call keeps its target as data rather than
/// an identifier, so the prefix walk above cannot see one — and a
/// classifier that tail-calls itself is still recursive.
fn mentions_name(expr: &Spanned<Expr>, name: &str) -> bool {
    let hit = match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n == name,
        Expr::TailCall(data) => data.target == name,
        _ => false,
    };
    if hit {
        return true;
    }
    let mut found = false;
    walk_children(expr, &mut |child| {
        found = found || mentions_name(child, name);
    });
    found
}

// ── Shared helpers ──────────────────────────────────────────────────

pub(super) fn fn_defs(items: &[TopLevel]) -> impl Iterator<Item = &FnDef> {
    items.iter().filter_map(|it| match it {
        TopLevel::FnDef(fd) => Some(fd),
        _ => None,
    })
}

pub(super) fn fn_defs_mut(items: &mut [TopLevel]) -> impl Iterator<Item = &mut FnDef> {
    items.iter_mut().filter_map(|it| match it {
        TopLevel::FnDef(fd) => Some(fd),
        _ => None,
    })
}

pub(super) fn stmt_expr(stmt: &Stmt) -> &Spanned<Expr> {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => e,
    }
}

pub(super) fn stmt_expr_mut(stmt: &mut Stmt) -> &mut Spanned<Expr> {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => e,
    }
}

/// Every name a synthesized `<fn>__cursor` must not collide with:
/// top-level items, and every name any function in the program binds.
///
/// The local half is not belt-and-braces. A call site is rewritten to
/// `<fn>__cursor` in the body it was found in, so a LOCAL of that name
/// in that body captures the call — the rewritten call reads the local
/// instead of the loop. Collecting locals program-wide rather than
/// per-body keeps one answer to "is this name free?", which is what the
/// synthesized name has to be.
pub(super) fn taken_names(items: &[TopLevel]) -> HashSet<String> {
    let mut out: HashSet<String> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd.name.clone()),
            TopLevel::TypeDef(
                crate::ast::TypeDef::Sum { name, .. } | crate::ast::TypeDef::Product { name, .. },
            ) => Some(name.clone()),
            TopLevel::Stmt(Stmt::Binding(name, _, _)) => Some(name.clone()),
            _ => None,
        })
        .collect();
    for fd in fn_defs(items) {
        collect_bound_names(fd, &mut out);
    }
    out
}

/// Every name `fd` introduces: parameters, statement bindings, and
/// pattern binders.
///
/// A binder is not a read, so no traversal that looks at identifiers can
/// see one — and a binder is exactly what shadows a name for the code
/// underneath it. Both questions this pass asks about names ("is the
/// `__cur_` namespace free in this loop?", "is `<fn>__cursor` free in
/// this program?") are questions about binders, so they share one
/// collector.
fn collect_bound_names(fd: &FnDef, out: &mut HashSet<String>) {
    out.extend(fd.params.iter().map(|(n, _)| n.clone()));
    for stmt in fd.body.stmts() {
        if let Stmt::Binding(name, _, _) = stmt {
            out.insert(name.clone());
        }
        collect_pattern_binders(stmt_expr(stmt), out);
    }
}

/// Every name a pattern under `expr` binds.
fn collect_pattern_binders(expr: &Spanned<Expr>, out: &mut HashSet<String>) {
    if let Expr::Match { arms, .. } = &expr.node {
        for arm in arms {
            out.extend(pattern_bindings(&arm.pattern));
        }
    }
    walk_children(expr, &mut |child| collect_pattern_binders(child, out));
}

fn is_list_of_string(ty: &str) -> bool {
    ty.replace(char::is_whitespace, "") == "List<String>"
}

/// Names a pattern binds — the shadowing set for its arm.
fn pattern_bindings(pattern: &Pattern) -> Vec<String> {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => vec![],
        Pattern::Ident(n) => vec![n.clone()],
        Pattern::Cons(head, tail) => vec![head.clone(), tail.clone()],
        Pattern::Tuple(items) => items.iter().flat_map(pattern_bindings).collect(),
        Pattern::Constructor(_, bindings) => bindings.clone(),
    }
}

/// Does any identifier under `expr` start with `prefix`?
fn mentions_prefix(expr: &Spanned<Expr>, prefix: &str) -> bool {
    let hit = match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.starts_with(prefix),
        _ => false,
    };
    if hit {
        return true;
    }
    let mut found = false;
    walk_children(expr, &mut |child| {
        found = found || mentions_prefix(child, prefix);
    });
    found
}

/// Visit every direct sub-expression. Exhaustive so a new `Expr`
/// variant has to be classified before this compiles.
pub(super) fn walk_children(expr: &Spanned<Expr>, f: &mut impl FnMut(&Spanned<Expr>)) {
    match &expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(base, _) | Expr::Neg(base) | Expr::ErrorProp(base) => f(base),
        Expr::FnCall(callee, args) => {
            f(callee);
            args.iter().for_each(&mut *f);
        }
        Expr::BinOp(_, lhs, rhs) => {
            f(lhs);
            f(rhs);
        }
        Expr::Match { subject, arms } => {
            f(subject);
            arms.iter().for_each(|a| f(&a.body));
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload {
                f(p);
            }
        }
        Expr::InterpolatedStr(parts) => parts.iter().for_each(|p| match p {
            StrPart::Literal(_) => {}
            StrPart::Parsed(e) => f(e),
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().for_each(&mut *f);
        }
        Expr::MapLiteral(entries) => entries.iter().for_each(|(k, v)| {
            f(k);
            f(v);
        }),
        Expr::RecordCreate { fields, .. } => fields.iter().for_each(|(_, e)| f(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            f(base);
            updates.iter().for_each(|(_, e)| f(e));
        }
        Expr::TailCall(data) => data.args.iter().for_each(&mut *f),
    }
}

/// Mutable mirror of [`walk_children`].
pub(super) fn walk_children_mut(expr: &mut Spanned<Expr>, f: &mut impl FnMut(&mut Spanned<Expr>)) {
    match &mut expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(base, _) | Expr::Neg(base) | Expr::ErrorProp(base) => f(base),
        Expr::FnCall(callee, args) => {
            f(callee);
            args.iter_mut().for_each(&mut *f);
        }
        Expr::BinOp(_, lhs, rhs) => {
            f(lhs);
            f(rhs);
        }
        Expr::Match { subject, arms } => {
            f(subject);
            arms.iter_mut().for_each(|a| f(&mut a.body));
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload {
                f(p);
            }
        }
        Expr::InterpolatedStr(parts) => parts.iter_mut().for_each(|p| match p {
            StrPart::Literal(_) => {}
            StrPart::Parsed(e) => f(e),
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter_mut().for_each(&mut *f);
        }
        Expr::MapLiteral(entries) => entries.iter_mut().for_each(|(k, v)| {
            f(k);
            f(v);
        }),
        Expr::RecordCreate { fields, .. } => fields.iter_mut().for_each(|(_, e)| f(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            f(base);
            updates.iter_mut().for_each(|(_, e)| f(e));
        }
        Expr::TailCall(data) => data.args.iter_mut().for_each(&mut *f),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    /// The whole program as one string. `unparse` cannot render the
    /// `Expr::TailCall` nodes TCO leaves behind, and these tests want to
    /// read the loops it rewrote, so the Debug form is the rendering
    /// that covers every node.
    fn rendered(items: &[TopLevel]) -> String {
        format!("{items:?}")
    }

    /// The one function, rendered.
    fn rendered_fn(items: &[TopLevel], name: &str) -> String {
        let fd = fn_defs(items)
            .find(|fd| fd.name == name)
            .unwrap_or_else(|| panic!("{name} is in the program"));
        format!("{fd:?}")
    }

    /// Parse + TCO, the state the pass expects to see.
    fn prepared(source: &str) -> Vec<TopLevel> {
        let mut items = crate::source::parse_source(source).expect("fixture parses");
        crate::ir::pipeline::tco(&mut items);
        items
    }

    fn fused(source: &str) -> (Vec<TopLevel>, CharsFusionPassReport) {
        let mut items = prepared(source);
        let report = run_chars_fusion_pass(&mut items);
        (items, report)
    }

    const DIGITS: &str = r#"module Digits
    intent =
        "Counts the decimal digits in a string."
    exposes [count]
    effects []

fn digitCount(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match head
            "0" -> digitCount(tail, acc + 1)
            "1" -> digitCount(tail, acc + 1)
            _ -> digitCount(tail, acc)

fn count(text: String) -> Int
    digitCount(String.chars(text), 0)
"#;

    #[test]
    fn a_linear_chars_loop_becomes_a_cursor() {
        let (items, report) = fused(DIGITS);
        assert_eq!(report.cursor_rewrites, 1, "one producer call site moves");
        assert_eq!(report.synthesized, vec!["digitCount__cursor".to_string()]);
        assert!(report.declined.is_empty(), "{:?}", report.declined);

        let variant = rendered_fn(&items, "digitCount__cursor");
        assert!(
            variant.contains("__str_cursor_end")
                && variant.contains("__str_cursor_head")
                && variant.contains("__str_cursor_next"),
            "the variant walks a cursor:\n{variant}"
        );
        assert!(
            !variant.contains("EmptyList") && !variant.contains("Cons("),
            "no list pattern survives in the variant:\n{variant}"
        );

        let caller = rendered_fn(&items, "count");
        assert!(
            caller.contains("digitCount__cursor")
                && caller.contains("Ident(\"text\")")
                && !caller.contains("String.chars")
                && !caller.contains("chars"),
            "the call site hands the string itself, not its characters:\n{caller}"
        );

        // The loop the user wrote is still exactly the loop the user
        // wrote — the variant is an addition, not a replacement.
        let (untouched, _) = (prepared(DIGITS), ());
        assert_eq!(
            rendered_fn(&items, "digitCount")
                .replace("__str_code1", "")
                .contains("EmptyList"),
            rendered_fn(&untouched, "digitCount").contains("EmptyList"),
            "the original loop keeps its list match"
        );
    }

    #[test]
    fn single_character_arms_become_codepoints() {
        let (items, report) = fused(DIGITS);
        // Once in the original loop, once in the synthesized variant.
        assert_eq!(report.codepoint_matches, 2);
        let all = rendered(&items);
        assert!(
            all.contains("__str_code1") && all.contains("Int(48)") && !all.contains("Str(\"0\")"),
            "the arms compare codepoints:\n{all}"
        );
    }

    #[test]
    fn a_case_folded_subject_folds_in_the_intrinsic() {
        let (items, report) = fused(
            r#"module Fold
    intent =
        "Reads one hex digit."
    exposes [value]
    effects []

fn value(digit: String) -> Int
    match String.toLower(digit)
        "a" -> 10
        _ -> -1
"#,
        );
        assert_eq!(report.codepoint_matches, 1);
        let value = rendered_fn(&items, "value");
        assert!(
            value.contains("__str_code1_lower")
                && !value.contains("toLower")
                && value.contains("Int(97)"),
            "toLower folds into the intrinsic instead of building a string:\n{value}"
        );
    }

    /// Each decline below is a loop that answers correctly today. The
    /// witness that it still does is in `tests/chars_fusion_declines.rs`;
    /// what is pinned here is that the recogniser says no, and why.
    fn decline_reason(source: &str, fn_name: &str) -> &'static str {
        let (_, report) = fused(source);
        assert_eq!(
            report.cursor_rewrites, 0,
            "this shape must not fuse: {:?}",
            report.synthesized
        );
        report
            .declined
            .get(fn_name)
            .copied()
            .unwrap_or_else(|| panic!("{fn_name} must be reported as declined: {report:?}"))
    }

    #[test]
    fn a_loop_that_measures_its_list_declines() {
        let reason = decline_reason(
            r#"module Measure
    intent =
        "Stops early when the remaining input is short."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match List.len(chars) > 2
            true -> walk(tail, acc + 1)
            false -> acc

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::ListEscapes.reason());
    }

    #[test]
    fn a_loop_that_hands_its_list_to_another_fn_declines() {
        let reason = decline_reason(
            r#"module Handoff
    intent =
        "Passes the remaining characters to a helper."
    exposes [run]
    effects []

fn rest(chars: List<String>) -> Int
    List.len(chars)

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + rest(tail))

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::ListEscapes.reason());
    }

    #[test]
    fn a_third_arm_declines() {
        let reason = decline_reason(
            r#"module ThreeArms
    intent =
        "Treats a one-element list specially."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)
        _ -> acc

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::MatchShape.reason());
    }

    #[test]
    fn a_recursive_call_that_restarts_the_list_declines() {
        let reason = decline_reason(
            r#"module Restart
    intent =
        "Recurses on a list it did not step."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match acc > 3
            true -> acc
            false -> walk([head], acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::RecursiveCallShape.reason());
    }

    /// A binder that reuses a tracked name takes it out of scope, so a
    /// read after the shadow is a read of the shadow. Here the loop
    /// recurses on a `tail` that is a DIFFERENT list — an empty one, so
    /// the loop stops after one character — while a rewrite that
    /// stepped the cursor on the strength of the name would walk the
    /// whole string. The name lies; the scope does not.
    #[test]
    fn a_shadowed_list_name_is_the_shadow_not_the_cursor() {
        let reason = decline_reason(
            r#"module Shadowed
    intent =
        "Recurses on a shorter list bound under the tail's own name."
    exposes [run]
    effects []

fn empty() -> List<String>
    []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match empty()
            tail -> walk(tail, acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::RecursiveCallShape.reason());
    }

    /// The other side of the same rule: a shadow the loop only READS is
    /// no reason to decline, because the rewrite never touches it.
    #[test]
    fn a_shadow_that_is_only_read_still_fuses() {
        let (items, report) = fused(
            r#"module ReadShadow
    intent =
        "Binds the head's own name over the tail while counting."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match [head]
            head -> walk(tail, acc + List.len(head))

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
        );
        assert_eq!(report.cursor_rewrites, 1);
        let variant = rendered_fn(&items, "walk__cursor");
        assert!(
            variant.contains("\"len\"") && variant.contains("__str_cursor_next"),
            "the shadow is still read as the list it is, and the real tail \
             still steps the cursor:\n{variant}"
        );
    }

    #[test]
    fn an_existing_cursor_name_declines() {
        let reason = decline_reason(
            r#"module Taken
    intent =
        "Already has a function named like the synthesized variant."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn walk__cursor(text: String, i: Int, acc: Int) -> Int
    acc

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::NameTaken.reason());
    }

    /// A statement binding spelled like the cursor parameter. It shadows
    /// that parameter for the whole body underneath, so the emitted end
    /// test would read the user's number — 99 is past the end of any
    /// short string, and a number inside the string is an infinite loop.
    #[test]
    fn a_binding_in_the_cursor_namespace_declines() {
        let reason = decline_reason(
            r#"module CursorBinding
    intent =
        "Binds the cursor parameter's own name."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    __cur_i = 99
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::NameTaken.reason());
    }

    /// The same capture through a pattern binder. A binder is not a
    /// read, so the identifier traversal cannot see it.
    #[test]
    fn a_pattern_binder_in_the_cursor_namespace_declines() {
        let reason = decline_reason(
            r#"module CursorPattern
    intent =
        "Binds the cursor parameter's own name in a pattern."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match acc + 3
        __cur_i -> match chars
            [] -> acc
            [head, ..tail] -> walk(tail, acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::NameTaken.reason());
    }

    /// The synthesized name is free at top level and bound as a LOCAL in
    /// the body whose call site would be rewritten to it. The rewritten
    /// call would read the local.
    #[test]
    fn a_local_holding_the_synthesized_name_declines() {
        let reason = decline_reason(
            r#"module CallerBinds
    intent =
        "The caller binds the name the pass would synthesize."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + 1)

fn run(text: String) -> Int
    walk__cursor = 42
    walk(String.chars(text), 0) + walk__cursor
"#,
            "walk",
        );
        assert_eq!(reason, CharsFusionDecline::NameTaken.reason());
    }

    /// The control for the three above: ordinary names in the same three
    /// places still fuse, so what those pin is the name and not the shape.
    #[test]
    fn the_same_three_places_under_ordinary_names_still_fuse() {
        let (_, report) = fused(
            r#"module OrdinaryNames
    intent =
        "A statement binding, a pattern binder and a caller local, none reserved."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    unrelated = 99
    match acc + 3
        alsoUnrelated -> match chars
            [] -> acc + unrelated - alsoUnrelated
            [head, ..tail] -> walk(tail, acc + 1)

fn run(text: String) -> Int
    spare = 42
    walk(String.chars(text), 0) + spare
"#,
        );
        assert_eq!(report.cursor_rewrites, 1, "{report:?}");
        assert!(report.declined.is_empty(), "{:?}", report.declined);
    }

    /// Every name the pass emits a call to. The recogniser declines the
    /// whole `__str_` namespace, and this walk keeps the list honest:
    /// a new intrinsic added to the pass belongs here the day it is
    /// emitted.
    const EMITTED_NAMES: [&str; 9] = [
        "__str_cursor_head",
        "__str_cursor_code",
        "__str_cursor_next",
        "__str_cursor_end",
        "__str_code1",
        "__str_code1_lower",
        "__str_code1_upper",
        "__str_fold_lower",
        "__str_fold_upper",
    ];

    /// A binder spelled like ANY emitted intrinsic, in any position a
    /// loop can bind a name — parameter, statement binding, pattern
    /// binder — declines the loop whole. The emitted calls are bare
    /// identifiers resolved after this pass, so a binder carrying one
    /// of these names captures the call for everything underneath it;
    /// the answer witnesses are in `tests/chars_fusion_declines.rs`,
    /// what is pinned here is that the recogniser says no, and why.
    #[test]
    fn a_binder_spelled_like_an_emitted_intrinsic_declines() {
        let templates = [
            (
                "parameter",
                r#"module ProbeParam
    intent =
        "Carries a spare number under a probed name."
    exposes [run]
    effects []

fn walk(chars: List<String>, BINDER: Int, acc: Int) -> Int
    match chars
        [] -> acc + BINDER
        [head, ..tail] -> walk(tail, BINDER, acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 100, 0)
"#,
            ),
            (
                "statement binding",
                r#"module ProbeStmt
    intent =
        "Binds a spare number under a probed name."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    BINDER = 100
    match chars
        [] -> acc + BINDER
        [head, ..tail] -> walk(tail, acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            ),
            (
                "pattern binder",
                r#"module ProbePattern
    intent =
        "Introduces a probed name in a pattern around the list match."
    exposes [run]
    effects []

fn walk(chars: List<String>, acc: Int) -> Int
    match acc + 100
        BINDER -> match chars
            [] -> BINDER
            [head, ..tail] -> walk(tail, acc + 1)

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
            ),
        ];
        for name in EMITTED_NAMES {
            for (position, template) in &templates {
                let reason = decline_reason(&template.replace("BINDER", name), "walk");
                assert_eq!(
                    reason,
                    CharsFusionDecline::NameTaken.reason(),
                    "a {position} binder named {name} must decline as a taken name"
                );
            }
        }
    }

    /// The codepoint rewrite emits `__str_code1*` calls into whatever
    /// function the match sits in — cursor loop or not — so a function
    /// that binds a name in the emitted namespace keeps its string
    /// match even when it is not a loop this pass would fuse.
    #[test]
    fn a_function_binding_the_emitted_namespace_keeps_its_string_match() {
        let (items, report) = fused(
            r#"module CodeName
    intent =
        "Matches characters beside a parameter carrying an intrinsic's name."
    exposes [rank]
    effects []

fn rank(word: String, __str_code1: Int) -> Int
    match word
        "a" -> 1
        _ -> __str_code1
"#,
        );
        assert_eq!(report.codepoint_matches, 0, "{report:?}");
        // The wildcard arm's read of the parameter is the program's own;
        // what must not appear is a CALL — the body has none, so any
        // `FnCall` here is an emitted intrinsic the parameter captures.
        let rank = rendered_fn(&items, "rank");
        assert!(
            rank.contains("Str(\"a\")") && !rank.contains("FnCall"),
            "the match stays on strings and no intrinsic call is emitted \
             where the parameter would capture it:\n{rank}"
        );
    }

    /// The control: the same function under an ordinary second
    /// parameter gets its codepoint match, so what the test above pins
    /// is the name and not the shape.
    #[test]
    fn the_same_function_under_an_ordinary_name_gets_its_codepoint_match() {
        let (_, report) = fused(
            r#"module CodeNameControl
    intent =
        "Matches characters beside an ordinarily named parameter."
    exposes [rank]
    effects []

fn rank(word: String, fallback: Int) -> Int
    match word
        "a" -> 1
        _ -> fallback
"#,
        );
        assert_eq!(report.codepoint_matches, 1, "{report:?}");
    }

    #[test]
    fn a_multi_character_arm_keeps_its_string_match() {
        let (_, report) = fused(
            r#"module Words
    intent =
        "Matches on words, not characters."
    exposes [value]
    effects []

fn value(word: String) -> Int
    match word
        "ab" -> 1
        "c" -> 2
        _ -> 0
"#,
        );
        assert_eq!(report.codepoint_matches, 0);
    }

    #[test]
    fn a_non_ascii_arm_keeps_its_string_match() {
        let (_, report) = fused(
            r#"module Accented
    intent =
        "Matches an accented character."
    exposes [value]
    effects []

fn value(c: String) -> Int
    match c
        "é" -> 1
        _ -> 0
"#,
        );
        assert_eq!(report.codepoint_matches, 0);
    }

    /// A default arm that BINDS holds the subject. Rewriting the
    /// subject to a codepoint would put an integer in a binding written
    /// to hold a string.
    #[test]
    fn a_binding_default_arm_keeps_its_string_match() {
        let (_, report) = fused(
            r#"module Echo
    intent =
        "Falls back to the character it was given."
    exposes [value]
    effects []

fn value(c: String) -> String
    match c
        "a" -> "A"
        other -> other
"#,
        );
        assert_eq!(report.codepoint_matches, 0);
    }

    #[test]
    fn a_match_without_a_default_keeps_its_string_match() {
        let (_, report) = fused(
            r#"module Total
    intent =
        "Covers its two characters and nothing else."
    exposes [value]
    effects []

fn value(c: String) -> Int
    match c
        "a" -> 1
        "b" -> 2
        "c" -> 3
"#,
        );
        assert_eq!(report.codepoint_matches, 0);
    }

    /// A cursor loop whose every read of the head is the argument of a
    /// small String classifier. The classifier grows a `__code` variant
    /// that takes the codepoint, and the loop binds the code instead of
    /// materialising a one-character string per step.
    const CLASSIFIED: &str = r#"module Classified
    intent =
        "Sums hex digit values through a single-character classifier."
    exposes [run]
    effects []

fn value(digit: String) -> Int
    match String.toLower(digit)
        "0" -> 0
        "1" -> 1
        "a" -> 10
        _ -> 0 - 1

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + value(head))

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#;

    #[test]
    fn a_classifier_call_in_a_cursor_loop_moves_to_the_codepoint() {
        let (items, report) = fused(CLASSIFIED);
        assert!(
            report.synthesized.contains(&"value__code".to_string()),
            "the classifier grows a codepoint variant: {report:?}"
        );
        assert_eq!(report.codepoint_calls, 1, "{report:?}");

        let variant = rendered_fn(&items, "walk__cursor");
        assert!(
            variant.contains("__str_cursor_code") && variant.contains("value__code"),
            "the loop binds the code and hands it across the call:\n{variant}"
        );
        assert!(
            !variant.contains("__str_cursor_head"),
            "no read is left that needs the one-character string:\n{variant}"
        );

        let code = rendered_fn(&items, "value__code");
        assert!(
            code.contains("__str_fold_lower") && code.contains("Int(48)"),
            "the variant folds case on the codepoint:\n{code}"
        );
        assert!(
            rendered_fn(&items, "value").contains("__str_code1_lower"),
            "the String classifier the user wrote is still there"
        );
    }

    /// The parseHexChars shape: the classifier consumes the character,
    /// and an error arm still prints it — from inside a LATER peel's
    /// scope, where the cursor has already stepped twice. The
    /// re-materialised head must read the offset the binding had, not
    /// the offset the loop has moved on to.
    const REPORTING: &str = r#"module Reporting
    intent =
        "Decides pairs of characters and reports the first bad one."
    exposes [run]
    effects []

fn value(digit: String) -> Int
    match digit
        "0" -> 0
        _ -> 0 - 1

fn pairs(chars: List<String>, acc: Int) -> String
    match chars
        [] -> "{acc}"
        [high, ..afterHigh] -> match afterHigh
            [] -> "odd"
            [low, ..rest] -> match value(high) < 0
                true -> "bad '{high}'"
                false -> pairs(rest, acc + value(low))

fn run(text: String) -> String
    pairs(String.chars(text), 0)
"#;

    #[test]
    fn a_cold_read_rematerialises_the_head_at_the_binds_own_offset() {
        let (items, report) = fused(REPORTING);
        assert_eq!(report.codepoint_calls, 2, "{report:?}");
        let variant = rendered_fn(&items, "pairs__cursor");
        assert!(
            variant.contains("__str_cursor_code") && variant.contains("value__code"),
            "both classifier calls take the code:\n{variant}"
        );
        // Exactly one head read survives: the interpolation in the
        // error arm.
        let head_at = variant
            .find("__str_cursor_head")
            .expect("the cold read re-materialises the head");
        assert!(
            !variant[head_at + 1..].contains("__str_cursor_head"),
            "only the cold read keeps a head:\n{variant}"
        );
        // The order pin. The read sits under the inner peel, where
        // `__cur_i1` and `__cur_i2` are in scope, and it must still ask
        // for the character at `__cur_i` — the offset its binding read.
        let window = &variant[head_at..variant.len().min(head_at + 220)];
        assert!(
            window.contains(r#"Ident("__cur_i")"#),
            "the re-materialised head reads the bind-time offset:\n{window}"
        );
    }

    /// The classifier's parameter is read in an arm body, so passing
    /// only the code would leave that read with nothing to read. No
    /// variant, no report — an almost-qualifying callee is a missed
    /// opportunity, not a decline.
    #[test]
    fn a_classifier_that_reads_its_parameter_elsewhere_keeps_the_string_call() {
        let (items, report) = fused(
            r#"module CalleeReads
    intent =
        "Falls back to the character it was handed."
    exposes [run]
    effects []

fn classify(c: String) -> String
    match c
        "a" -> "letter"
        _ -> c

fn walk(chars: List<String>, acc: String) -> String
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + classify(head))

fn run(text: String) -> String
    walk(String.chars(text), "")
"#,
        );
        assert!(
            !report.synthesized.iter().any(|n| n.ends_with("__code")),
            "{report:?}"
        );
        assert_eq!(report.codepoint_calls, 0);
        assert!(report.declined.is_empty(), "{:?}", report.declined);
        let variant = rendered_fn(&items, "walk__cursor");
        assert!(
            variant.contains("__str_cursor_head") && !variant.contains("__str_cursor_code"),
            "the loop still hands the classifier its string:\n{variant}"
        );
    }

    /// A self-call inside the classifier body means the variant would
    /// have to rewrite recursion it has no cursor for. No variant.
    #[test]
    fn a_recursive_classifier_keeps_the_string_call() {
        let (_, report) = fused(
            r#"module RecursiveCallee
    intent =
        "Retries itself on the wildcard arm."
    exposes [run]
    effects []

fn value(digit: String) -> Int
    match digit
        "0" -> 0
        _ -> value("0")

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + value(head))

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
        );
        assert!(
            !report.synthesized.iter().any(|n| n.ends_with("__code")),
            "{report:?}"
        );
        assert_eq!(report.codepoint_calls, 0);
    }

    /// The name the variant would take is already a local somewhere, so
    /// a rewritten call could be captured. No variant.
    #[test]
    fn a_taken_code_name_blocks_the_variant() {
        let (_, report) = fused(
            r#"module TakenCode
    intent =
        "Binds the name the pass would synthesize for the classifier."
    exposes [run]
    effects []

fn value(digit: String) -> Int
    match digit
        "0" -> 0
        _ -> 0 - 1

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + value(head))

fn run(text: String) -> Int
    value__code = 42
    walk(String.chars(text), 0) + value__code
"#,
        );
        assert!(
            !report.synthesized.iter().any(|n| n.ends_with("__code")),
            "{report:?}"
        );
        assert_eq!(report.codepoint_calls, 0);
    }

    /// Only the call that receives the head moves to the code; a call
    /// with any other argument keeps the String classifier, which stays
    /// in the program precisely for that.
    #[test]
    fn a_classifier_argument_that_is_not_the_head_keeps_the_string_call() {
        let (items, report) = fused(
            r#"module MixedArgs
    intent =
        "Classifies the head and a constant in the same step."
    exposes [run]
    effects []

fn value(digit: String) -> Int
    match digit
        "0" -> 0
        _ -> 0 - 1

fn walk(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + value(head) + value("0"))

fn run(text: String) -> Int
    walk(String.chars(text), 0)
"#,
        );
        assert_eq!(report.codepoint_calls, 1, "{report:?}");
        let variant = rendered_fn(&items, "walk__cursor");
        assert!(
            variant.contains("value__code") && variant.contains("Ident(\"value\")"),
            "one call moves, the other stays:\n{variant}"
        );
    }

    #[test]
    fn a_program_without_chars_is_untouched() {
        let source = r#"module Plain
    intent =
        "Adds two numbers."
    exposes [add]
    effects []

fn add(a: Int, b: Int) -> Int
    a + b
"#;
        let before = prepared(source);
        let (after, report) = fused(source);
        assert_eq!(report, CharsFusionPassReport::default());
        assert_eq!(rendered(&before), rendered(&after));
    }
}
