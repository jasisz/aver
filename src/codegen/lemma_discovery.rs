//! Lemma discovery — the "locksmith" (Phase 2 of the charter,
//! `prompts/lemma-discovery.md`).
//!
//! Where the legacy `AccumulatorRoundtrip` recognizer was a *key* cut for one
//! lock (it fires on exactly `rle.av`), this is the *locksmith*: a pass that
//! discovers the auxiliary lemmas an inductive proof needs, proves them, and
//! emits them as explicit checkable artifacts. The full pipeline is:
//!
//! ```text
//!   LawProofCone  ─►  typed-term enumerator  ─►  VM-filter  ─►  backend-prove  ─►  commit
//!   (scope: pure       (small equations over     (Aver VM as    (Lean = truth,     (named .lean/
//!    fns + ADTs)         the cone, bounded by      test oracle,   Dafny = regression) .dfy + manifest)
//!                        term SIZE) + LLM          conservative
//!                        conjecturer (guarded)     on overflow)
//! ```
//!
//! The cone (built by `LawProofCone::compute`) is the differentiator: the
//! compiler already knows a law's scope, so the enumerator gets
//! goal-direction *for free* — external tools (HipSpec/CCLemma/…) must
//! reconstruct scope at cost.
//!
//! # What's implemented here (Phase 2a — skeleton)
//!
//! The structure + the *seed* layer of the type-directed term enumerator: for
//! each `verify ... law`, build its `LawProofCone`, read the cone's type
//! alphabet, and enumerate the depth-1 application of every cone fn to fresh
//! typed variables. No equation pairing, no VM-filter, no LLM, no backend
//! prove, no commit yet — those land in 2b–2e. Entry point is
//! [`run_discovery`], invoked by `aver proof --discover`; normal `aver proof`
//! never runs this (discovery is the explicit, expensive, cached step).

use crate::ast::{TopLevel, VerifyKind};
use crate::codegen::proof_lower::{LawProofCone, ProofLowerInputs};
use crate::types::Type;

/// A free variable in a typed term — a source-renderable name plus its Aver
/// type. `TermNode::Var(i)` in the owning [`TypedTerm`] refers to the binder
/// at index `i`.
#[derive(Debug, Clone)]
pub struct Binder {
    pub name: String,
    pub ty: Type,
}

/// A node in a typed term tree.
#[derive(Debug, Clone)]
pub enum TermNode {
    /// A bound variable, by index into the owning term's `binders`.
    Var(usize),
    /// Application of a cone pure fn (later also builtin ops) to typed args.
    App { callee: String, args: Vec<TermNode> },
}

/// A well-typed term over a law's proof cone. `ty` is the term's *result*
/// type; `binders` are its free variables. The enumeration bound the charter
/// mandates is term **size** (node count, [`TypedTerm::size`]) — not
/// arity × depth — so that `f(g(x))` and `f(x, y, z)` are weighed on the same
/// scale.
#[derive(Debug, Clone)]
pub struct TypedTerm {
    pub binders: Vec<Binder>,
    pub root: TermNode,
    pub ty: Type,
}

impl TermNode {
    fn size(&self) -> usize {
        match self {
            TermNode::Var(_) => 1,
            TermNode::App { args, .. } => 1 + args.iter().map(TermNode::size).sum::<usize>(),
        }
    }

    fn render(&self, binders: &[Binder]) -> String {
        match self {
            TermNode::Var(i) => binders
                .get(*i)
                .map(|b| b.name.clone())
                .unwrap_or_else(|| format!("?{i}")),
            TermNode::App { callee, args } => {
                let rendered: Vec<String> = args.iter().map(|a| a.render(binders)).collect();
                format!("{callee}({})", rendered.join(", "))
            }
        }
    }
}

impl TypedTerm {
    /// Node count — the enumeration size bound.
    pub fn size(&self) -> usize {
        self.root.size()
    }

    /// Source-shaped rendering, e.g. `decode(v0_0)`.
    pub fn render(&self) -> String {
        self.root.render(&self.binders)
    }
}

/// A discovery report for one `verify ... law`. The skeleton fills the cone
/// summary + the seed term layer; later phases extend it with candidate
/// equations, VM-filter verdicts, and proved lemmas.
#[derive(Debug, Clone)]
pub struct LawDiscovery {
    /// The law's subject fn (`verify <fn> law <name>`); excluded from the cone.
    pub subject_fn: String,
    /// The law's name.
    pub law_name: String,
    /// The cone vocabulary — pure fns the enumerator may apply (sorted).
    pub cone_fns: Vec<String>,
    /// The cone type alphabet — user ADTs reachable from those fns (sorted).
    pub cone_types: Vec<String>,
    /// The seed layer of enumerated typed terms (depth-1 cone-fn applications).
    pub seed_terms: Vec<TypedTerm>,
}

/// Run lemma discovery (skeleton) over every `verify ... law` in the entry
/// module: build each law's [`LawProofCone`] and enumerate the seed term
/// layer. Pure analysis — no VM, no prover, no file writes.
pub fn run_discovery(inputs: &ProofLowerInputs) -> Vec<LawDiscovery> {
    let mut reports = Vec::new();
    for item in inputs.entry_items {
        let TopLevel::Verify(vb) = item else {
            continue;
        };
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let cone = LawProofCone::compute(law, &vb.fn_name, inputs);
        reports.push(LawDiscovery {
            subject_fn: vb.fn_name.clone(),
            law_name: law.name.clone(),
            cone_fns: cone.pure_fns().iter().map(|fd| fd.name.clone()).collect(),
            cone_types: cone
                .types()
                .iter()
                .map(|td| crate::codegen::common::type_def_name(td).to_string())
                .collect(),
            seed_terms: seed_terms(&cone),
        });
    }
    reports
}

/// Build the size-bounded **seed** layer of typed terms over a cone: one
/// depth-1 application per cone pure fn, applied to fresh distinct variables
/// of its parameter types. This is the base of the (charter) size-bounded
/// enumeration — 2b grows it by composing seeds up to a size budget; the
/// skeleton stops here. Deterministic: cone fns are already sorted by name.
fn seed_terms(cone: &LawProofCone) -> Vec<TypedTerm> {
    let mut out = Vec::new();
    for (fi, fd) in cone.pure_fns().iter().enumerate() {
        let mut binders = Vec::new();
        let mut args = Vec::new();
        for (pi, (_param_name, annotation)) in fd.params.iter().enumerate() {
            let ty = crate::codegen::common::parse_type_annotation(annotation);
            args.push(TermNode::Var(binders.len()));
            binders.push(Binder {
                name: format!("v{fi}_{pi}"),
                ty,
            });
        }
        out.push(TypedTerm {
            binders,
            root: TermNode::App {
                callee: fd.name.clone(),
                args,
            },
            ty: crate::codegen::common::parse_type_annotation(&fd.return_type),
        });
    }
    out
}

/// Render an Aver [`Type`] back to source-shaped text (`List<Run>`,
/// `Result<T, String>`, `(A, B)`, …) for the `--discover` report.
fn render_type(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(a, b) => format!("Result<{}, {}>", render_type(a), render_type(b)),
        Type::Option(a) => format!("Option<{}>", render_type(a)),
        Type::List(a) => format!("List<{}>", render_type(a)),
        Type::Vector(a) => format!("Vector<{}>", render_type(a)),
        Type::Map(a, b) => format!("Map<{}, {}>", render_type(a), render_type(b)),
        Type::Tuple(items) => format!(
            "({})",
            items.iter().map(render_type).collect::<Vec<_>>().join(", ")
        ),
        Type::Fn(args, ret, _) => format!(
            "({}) -> {}",
            args.iter().map(render_type).collect::<Vec<_>>().join(", "),
            render_type(ret)
        ),
        Type::Named { name, .. } => name.clone(),
        Type::Var(n) => n.clone(),
        Type::Invalid => "<invalid>".to_string(),
    }
}

/// Human-readable multi-line report for `aver proof --discover` output.
pub fn render_report(reports: &[LawDiscovery]) -> String {
    let mut out = String::new();
    if reports.is_empty() {
        out.push_str("lemma discovery: no `verify ... law` blocks found\n");
        return out;
    }
    out.push_str(&format!(
        "lemma discovery (skeleton): {} law(s)\n",
        reports.len()
    ));
    for r in reports {
        out.push_str(&format!("\n• verify {} law {}\n", r.subject_fn, r.law_name));
        out.push_str(&format!("    cone fns:   [{}]\n", r.cone_fns.join(", ")));
        out.push_str(&format!("    cone types: [{}]\n", r.cone_types.join(", ")));
        out.push_str(&format!("    seed terms ({}):\n", r.seed_terms.len()));
        for t in &r.seed_terms {
            out.push_str(&format!(
                "      {} : {}  (size {})\n",
                t.render(),
                render_type(&t.ty),
                t.size()
            ));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::ModuleInfo;
    use std::collections::HashSet;

    /// Minimal RLE-shaped fixture: a `decode` recursor over `List<Run>` with
    /// a transitive helper chain (`decode → expandRun → repeat`) and a
    /// roundtrip law whose subject is `encode`. Exercises the cone's
    /// fn-closure, type alphabet, and the enumerator seed layer.
    const SRC: &str = r#"
record Run
    char: String
    count: Int

fn repeat(c: String, n: Int) -> List<String>
    [c]

fn expandRun(r: Run) -> List<String>
    repeat(r.char, r.count)

fn decode(runs: List<Run>) -> List<String>
    match runs
        [] -> []
        [run, ..rest] -> List.concat(expandRun(run), decode(rest))

fn encode(xs: List<String>) -> List<Run>
    []

verify encode law roundtrip
    given xs: List<String> = [[], ["a"]]
    decode(encode(xs)) => xs
"#;

    fn discover(src: &str) -> Vec<LawDiscovery> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let items = crate::parser::Parser::new(tokens).parse().expect("parse");
        // `LawProofCone::compute` walks the AST directly (it does not touch
        // the symbol table), so a parse-only fixture is sufficient and
        // hermetic — no typecheck / resolve needed.
        let symbols = crate::ir::SymbolTable::build(&items, &[]);
        let prefixes: HashSet<String> = HashSet::new();
        let recursive: HashSet<crate::ir::FnId> = HashSet::new();
        let no_modules: &[ModuleInfo] = &[];
        let inputs = ProofLowerInputs {
            entry_items: &items,
            dep_modules: no_modules,
            module_prefixes: &prefixes,
            recursive_fns: &recursive,
            symbol_table: &symbols,
            program_shape: None,
        };
        run_discovery(&inputs)
    }

    #[test]
    fn cone_excludes_subject_and_closes_over_pure_helpers() {
        let reports = discover(SRC);
        assert_eq!(reports.len(), 1);
        let r = &reports[0];
        assert_eq!(r.subject_fn, "encode");
        assert_eq!(r.law_name, "roundtrip");
        // `encode` (subject) is dropped; `decode` + its transitive pure
        // helpers stay, sorted by name.
        assert_eq!(r.cone_fns, vec!["decode", "expandRun", "repeat"]);
    }

    #[test]
    fn cone_types_resolve_adts_from_signatures() {
        let r = &discover(SRC)[0];
        // `Run` is reachable from `decode`/`expandRun` signatures; builtin
        // scalars (`String`/`Int`) and collection ctors drop out.
        assert_eq!(r.cone_types, vec!["Run"]);
    }

    #[test]
    fn seed_layer_enumerates_trivial_cone_term() {
        let r = &discover(SRC)[0];
        // One depth-1 application per cone fn, in sorted order.
        assert_eq!(r.seed_terms.len(), 3);
        let decode_term = &r.seed_terms[0];
        assert_eq!(decode_term.render(), "decode(v0_0)");
        assert_eq!(render_type(&decode_term.ty), "List<String>");
        // `decode(v0_0)` = one application node + one variable node.
        assert_eq!(decode_term.size(), 2);
    }
}
