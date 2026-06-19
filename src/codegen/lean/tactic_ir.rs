//! Structured Lean tactic-combinator tree — the proof-output substrate.
//!
//! Auto-proofs are assembled today as raw `first | (…) | (…) | sorry` STRINGS
//! (`AutoProof.proof_lines`). Every rung KNOWS its portfolio of alternatives,
//! then immediately flattens it to a string — which forces any later
//! proof-output pass (`--minimize`, marker instrumentation, `--explain`) to
//! re-parse the multi-line, nested Lean it just produced. That round-trip is
//! the brittleness.
//!
//! This thin tree keeps the CONTROL structure — sequencing, `first`
//! alternation, induction arms — first-class. Leaves stay opaque tactic text
//! (`simp only […] <;> omega`, `grind […]; done`, `exact …`): we model how a
//! proof is *assembled*, not Lean's tactic semantics. With the structure
//! retained, `--minimize` collapses a [`Tactic::First`] to its winning branch
//! STRUCTURALLY (pick a child, re-print), never by text surgery; the only thing
//! that still has to consult Lean is *which* branch won — and that is one
//! instrumented `lake build`, not a parser.
// Foundation module: the type + printer land first, then the ~18 `first | …`
// emit sites migrate onto it and `--minimize` consumes it. Allow dead_code
// until those consumers are wired (next slice).
#![allow(dead_code)]

use std::collections::BTreeMap;

/// A Lean tactic, modelled at the control level only.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tactic {
    /// One opaque tactic. May contain `;` / `<;>` internally — not modelled.
    Leaf(String),
    /// The `sorry` floor — rendered bare (`sorry`), never parenthesised.
    Sorry,
    /// A `by`-block sequence: each step rendered on its own line, in order.
    Seq(Vec<Tactic>),
    /// `first | b₁ | b₂ | …` — the portfolio a minimizer collapses to one
    /// branch. Lean commits to the leftmost branch that closes, so the winner
    /// reported by the marker build is exactly the branch to keep.
    First(Vec<Tactic>),
    /// `induction <target> with` + one arm per variant. Arm bodies are
    /// themselves tactics (they routinely contain their own [`Tactic::First`]).
    Induction {
        target: String,
        arms: Vec<InductionArm>,
    },
}

/// One `| <pattern> => <body>` arm of an [`Tactic::Induction`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InductionArm {
    /// The pattern after `|` and before `=>`, e.g. `nil` or `cons head tail ih`.
    pub pattern: String,
    pub body: Tactic,
}

impl Tactic {
    /// Wrap already-rendered proof lines as an opaque sequence — the
    /// behavior-preserving bridge for proofs not yet structured into `First`
    /// nodes. `raw(lines).render() == lines`, so migrating a site to
    /// `body: Tactic::raw(<old proof_lines>)` is a no-op on the emitted Lean;
    /// only the portfolio sites that later become real [`Tactic::First`] trees
    /// gain anything for `--minimize`.
    pub fn raw(lines: Vec<String>) -> Tactic {
        Tactic::Seq(lines.into_iter().map(Tactic::Leaf).collect())
    }

    /// Like [`raw`](Self::raw) but first strips the lines' common leading
    /// indent (keeping relative nesting). Use when wrapping already-rendered
    /// lines as a [`Tactic::First`] BRANCH: a branch is re-based under its `| (`
    /// at render time, and when `--minimize` collapses the portfolio the branch
    /// is promoted flush with its new siblings — both want it authored at its
    /// own zero indent, not carrying the caller's baked 2-space.
    pub fn raw_dedented(lines: Vec<String>) -> Tactic {
        Tactic::raw(relative_dedent(&lines))
    }

    /// Render to the proof-body lines that follow `:= by` (the caller supplies
    /// the theorem-level indent when it stitches these into the file). Produces
    /// valid Lean; formatting is normalised (it need not be byte-identical to
    /// the legacy string emit — the contract is that the proof still closes).
    pub fn render(&self) -> Vec<String> {
        self.render_indent(0)
    }

    /// Render the proof body as it sits under a theorem's `:= by` — at the
    /// canonical 2-space indent.
    ///
    /// `raw`-migrated bodies carry a baked-in leading indent in their leaf
    /// strings (the legacy emit indented every proof line by 2). Re-indenting
    /// that on top would double it, so this first strips the tree's common
    /// leaf indent (preserving any deeper *relative* nesting) and then renders
    /// at indent 1. For a uniformly 2-space-baked body that is byte-identical
    /// to the legacy output; for a structured body (a real [`Tactic::First`]
    /// authored without baked indent) it lays the `first`/`|` keywords at the
    /// 2-space column the surrounding `intro` sits at — which Lean's
    /// column-sensitive `by` block requires.
    pub fn render_body(&self) -> Vec<String> {
        // Under an active `--minimize` pass (thread-local, set by `cmd_proof`),
        // rewrite the tree first: Instrument prefixes each `First` branch with a
        // winner-probe marker; Collapse drops each `First` to its proven winner.
        // Outside a minimize pass this is a no-op clone.
        let tree = minimize::apply(self);
        let strip = tree.leaf_min_indent().unwrap_or(0);
        tree.strip_leaf_indent(strip).render_indent(1)
    }

    /// The smallest leading-space count across every non-blank line of every
    /// leaf in the tree (`None` if the tree has no non-blank leaf line).
    /// Structural keywords (`first`, `| …`, `induction … with`) are NOT leaves
    /// and do not count — only authored tactic text does.
    fn leaf_min_indent(&self) -> Option<usize> {
        match self {
            Tactic::Leaf(s) => s
                .lines()
                .filter(|l| !l.trim().is_empty())
                .map(leading_spaces)
                .min(),
            Tactic::Sorry => None,
            Tactic::Seq(ts) | Tactic::First(ts) => {
                ts.iter().filter_map(Tactic::leaf_min_indent).min()
            }
            Tactic::Induction { arms, .. } => {
                arms.iter().filter_map(|a| a.body.leaf_min_indent()).min()
            }
        }
    }

    /// Strip up to `n` leading spaces from every line of every leaf — the
    /// un-bake step paired with [`render_body`]. Clamped per line so a line
    /// shallower than `n` is left flush, never over-stripped into its content.
    fn strip_leaf_indent(self, n: usize) -> Tactic {
        match self {
            Tactic::Leaf(s) => Tactic::Leaf(
                s.lines()
                    .map(|l| {
                        let k = leading_spaces(l).min(n);
                        l[k..].to_string()
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
            ),
            Tactic::Sorry => Tactic::Sorry,
            Tactic::Seq(ts) => {
                Tactic::Seq(ts.into_iter().map(|t| t.strip_leaf_indent(n)).collect())
            }
            Tactic::First(ts) => {
                Tactic::First(ts.into_iter().map(|t| t.strip_leaf_indent(n)).collect())
            }
            Tactic::Induction { target, arms } => Tactic::Induction {
                target,
                arms: arms
                    .into_iter()
                    .map(|a| InductionArm {
                        pattern: a.pattern,
                        body: a.body.strip_leaf_indent(n),
                    })
                    .collect(),
            },
        }
    }

    fn render_indent(&self, indent: usize) -> Vec<String> {
        let pad = "  ".repeat(indent);
        match self {
            // An empty leaf stays a truly empty line — never padded. (A
            // `String::new()` step in a `raw`-wrapped proof is a blank
            // separator; emitting `pad` instead would leave trailing
            // whitespace and break byte-identity when rendered at depth.)
            Tactic::Leaf(s) if s.is_empty() => vec![String::new()],
            // Preserve an empty leaf as an empty line (`"".lines()` yields
            // NOTHING, which would silently drop blank lines from `raw`-wrapped
            // proofs); only split a genuinely multi-line leaf.
            Tactic::Leaf(s) if !s.contains('\n') => vec![format!("{pad}{s}")],
            Tactic::Leaf(s) => s.lines().map(|l| format!("{pad}{l}")).collect(),
            Tactic::Sorry => vec![format!("{pad}sorry")],
            Tactic::Seq(steps) => steps.iter().flat_map(|t| t.render_indent(indent)).collect(),
            Tactic::First(branches) => render_first(branches, indent),
            Tactic::Induction { target, arms } => {
                let mut out = vec![format!("{pad}induction {target} with")];
                for arm in arms {
                    // `| pat =>` then the body inline if single-line, else the
                    // body indented under the arm.
                    let body = arm.body.render_indent(indent + 1);
                    if body.len() == 1 {
                        out.push(format!(
                            "{pad}| {} => {}",
                            arm.pattern,
                            body[0].trim_start()
                        ));
                    } else {
                        out.push(format!("{pad}| {} =>", arm.pattern));
                        out.extend(body);
                    }
                }
                out
            }
        }
    }

    /// The `--minimize` primitive: walk the tree, and for each [`Tactic::First`]
    /// ask `pick` which branch won (by the branch list); `Some(i)` collapses the
    /// portfolio to branch `i` (recursively minimized), `None` keeps it intact.
    /// `pick` is fed the [`Tactic::First`] nodes in pre-order, so a marker pass
    /// that numbered them in the same order can answer by index.
    pub fn collapse_firsts(self, pick: &mut impl FnMut(&[Tactic]) -> Option<usize>) -> Tactic {
        match self {
            leaf @ (Tactic::Leaf(_) | Tactic::Sorry) => leaf,
            Tactic::Seq(steps) => {
                Tactic::Seq(steps.into_iter().map(|t| t.collapse_firsts(pick)).collect())
            }
            Tactic::First(branches) => match pick(&branches) {
                Some(i) if i < branches.len() => {
                    branches.into_iter().nth(i).unwrap().collapse_firsts(pick)
                }
                _ => Tactic::First(
                    branches
                        .into_iter()
                        .map(|t| t.collapse_firsts(pick))
                        .collect(),
                ),
            },
            Tactic::Induction { target, arms } => Tactic::Induction {
                target,
                arms: arms
                    .into_iter()
                    .map(|a| InductionArm {
                        pattern: a.pattern,
                        body: a.body.collapse_firsts(pick),
                    })
                    .collect(),
            },
        }
    }

    /// Count of [`Tactic::First`] nodes, in pre-order — the number of marker
    /// sites the instrument pass will emit and the winner pass will read back.
    pub fn first_count(&self) -> usize {
        match self {
            Tactic::Leaf(_) | Tactic::Sorry => 0,
            Tactic::Seq(ts) => ts.iter().map(Tactic::first_count).sum(),
            Tactic::First(bs) => 1 + bs.iter().map(Tactic::first_count).sum::<usize>(),
            Tactic::Induction { arms, .. } => arms.iter().map(|a| a.body.first_count()).sum(),
        }
    }

    /// Instrument every [`Tactic::First`] for the `--minimize` winner probe:
    /// prefix each branch with a `trace "AVERMIN:<idx>:<b>"` marker, where
    /// `<idx>` is the node's global pre-order index (drawn from `next`) and
    /// `<b>` is the branch position. Lean's `first` runs branches left-to-right
    /// and commits to the first that closes, tracing each it tries — so after
    /// one instrumented `lake build` the WINNING branch of node `idx` is the
    /// MAX `<b>` that surfaced (failed branches trace too; they are not rolled
    /// back). Indices are assigned by a pure structural walk so this pass and
    /// [`collapse_by_index`](Self::collapse_by_index) agree node-for-node.
    fn instrument_markers(self, next: &mut usize) -> Tactic {
        match self {
            leaf @ (Tactic::Leaf(_) | Tactic::Sorry) => leaf,
            Tactic::Seq(ts) => {
                Tactic::Seq(ts.into_iter().map(|t| t.instrument_markers(next)).collect())
            }
            Tactic::First(branches) => {
                let idx = *next;
                *next += 1;
                Tactic::First(
                    branches
                        .into_iter()
                        .enumerate()
                        .map(|(b, branch)| {
                            // Recurse before prepending the marker so nested
                            // `First`s take indices AFTER this node (pre-order).
                            let inner = branch.instrument_markers(next);
                            Tactic::Seq(vec![
                                Tactic::Leaf(format!("trace \"AVERMIN:{idx}:{b}\"")),
                                inner,
                            ])
                        })
                        .collect(),
                )
            }
            Tactic::Induction { target, arms } => Tactic::Induction {
                target,
                arms: arms
                    .into_iter()
                    .map(|a| InductionArm {
                        pattern: a.pattern,
                        body: a.body.instrument_markers(next),
                    })
                    .collect(),
            },
        }
    }

    /// Collapse each [`Tactic::First`] to its winning branch per `winners`
    /// (keyed by the SAME global pre-order index
    /// [`instrument_markers`](Self::instrument_markers) assigned). A node absent
    /// from the map — never executed in the probe build, so no marker surfaced —
    /// is left intact. Walks ALL branches even when collapsing, so `next`
    /// advances exactly as in the instrument pass and downstream indices stay
    /// aligned; only the chosen branch's rewrite is kept.
    fn collapse_by_index(self, next: &mut usize, winners: &BTreeMap<usize, usize>) -> Tactic {
        match self {
            leaf @ (Tactic::Leaf(_) | Tactic::Sorry) => leaf,
            Tactic::Seq(ts) => Tactic::Seq(
                ts.into_iter()
                    .map(|t| t.collapse_by_index(next, winners))
                    .collect(),
            ),
            Tactic::First(branches) => {
                let idx = *next;
                *next += 1;
                // An out-of-range winner (should never happen) degrades to
                // "keep the whole portfolio" rather than dropping every branch.
                let n = branches.len();
                let winner = winners.get(&idx).copied().filter(|&w| w < n);
                let mut chosen = None;
                let mut kept: Vec<Tactic> = Vec::with_capacity(branches.len());
                for (b, branch) in branches.into_iter().enumerate() {
                    let collapsed = branch.collapse_by_index(next, winners);
                    match winner {
                        Some(w) if w == b => chosen = Some(collapsed),
                        Some(_) => {}
                        None => kept.push(collapsed),
                    }
                }
                match chosen {
                    Some(t) => t,
                    None => Tactic::First(kept),
                }
            }
            Tactic::Induction { target, arms } => Tactic::Induction {
                target,
                arms: arms
                    .into_iter()
                    .map(|a| InductionArm {
                        pattern: a.pattern,
                        body: a.body.collapse_by_index(next, winners),
                    })
                    .collect(),
            },
        }
    }
}

/// Count of leading ASCII spaces on a line.
fn leading_spaces(l: &str) -> usize {
    l.len() - l.trim_start().len()
}

/// Strip the common leading indent shared by all non-blank lines, preserving
/// each line's *relative* nesting (blank lines stay blank). Used to re-base a
/// multi-line `first` branch under its `| (` wrapper without flattening the
/// branch's own internal structure (an `induction`/`cases` ladder).
fn relative_dedent(lines: &[String]) -> Vec<String> {
    let min = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| leading_spaces(l))
        .min()
        .unwrap_or(0);
    lines
        .iter()
        .map(|l| {
            if l.trim().is_empty() {
                String::new()
            } else {
                l[min..].to_string()
            }
        })
        .collect()
}

/// Render a `First`: inline (`first | (b₀) | (b₁) | sorry`) when every branch is
/// a single line, else multi-line with each branch on its own `|` line.
fn render_first(branches: &[Tactic], indent: usize) -> Vec<String> {
    let pad = "  ".repeat(indent);
    let rendered: Vec<Vec<String>> = branches.iter().map(|b| b.render_indent(0)).collect();
    let all_single = rendered.iter().all(|b| b.len() == 1);
    if all_single {
        let parts: Vec<String> = branches
            .iter()
            .zip(&rendered)
            .map(|(b, lines)| match b {
                Tactic::Sorry => "sorry".to_string(),
                _ => format!("({})", lines[0].trim_start()),
            })
            .collect();
        vec![format!("{pad}first | {}", parts.join(" | "))]
    } else {
        let mut out = vec![format!("{pad}first")];
        for (b, lines) in branches.iter().zip(&rendered) {
            match b {
                Tactic::Sorry => out.push(format!("{pad}| sorry")),
                _ if lines.len() == 1 => out.push(format!("{pad}| ({})", lines[0].trim_start())),
                _ => {
                    out.push(format!("{pad}| ("));
                    // Re-base the branch relative to `| (`, keeping its own
                    // internal nesting (trimming each line would flatten an
                    // induction ladder inside the branch).
                    for l in relative_dedent(lines) {
                        if l.is_empty() {
                            out.push(String::new());
                        } else {
                            out.push(format!("{pad}  {l}"));
                        }
                    }
                    out.push(format!("{pad})"));
                }
            }
        }
        out
    }
}

/// The `--minimize` driver state, thread-local so the two re-emit passes
/// (instrument, then collapse) can steer [`Tactic::render_body`] without
/// threading a mode + counter through every codegen signature. Codegen runs
/// single-threaded per `transpile`, and a normal `aver proof` never enters a
/// pass, so the default ([`Mode::Off`]) leaves emission untouched.
pub mod minimize {
    use super::{BTreeMap, Tactic};
    use std::cell::RefCell;

    #[derive(Clone)]
    enum Mode {
        Off,
        Instrument,
        Collapse(BTreeMap<usize, usize>),
    }

    thread_local! {
        // (mode, global pre-order `First` counter advanced across all bodies).
        static STATE: RefCell<(Mode, usize)> = const { RefCell::new((Mode::Off, 0)) };
    }

    /// Enter the instrument pass: emit winner-probe markers, counter reset to 0.
    pub fn begin_instrument() {
        STATE.with(|s| *s.borrow_mut() = (Mode::Instrument, 0));
    }

    /// Enter the collapse pass with the parsed winners, counter reset to 0.
    pub fn begin_collapse(winners: BTreeMap<usize, usize>) {
        STATE.with(|s| *s.borrow_mut() = (Mode::Collapse(winners), 0));
    }

    /// Leave the minimize pass — emission returns to byte-for-byte normal.
    pub fn end() {
        STATE.with(|s| *s.borrow_mut() = (Mode::Off, 0));
    }

    /// Apply the active pass's rewrite to `t`, advancing the shared counter.
    /// Returns a plain clone when no pass is active.
    pub(super) fn apply(t: &Tactic) -> Tactic {
        STATE.with(|s| {
            let mut st = s.borrow_mut();
            let mode = st.0.clone();
            let mut counter = st.1;
            let out = match mode {
                Mode::Off => t.clone(),
                Mode::Instrument => t.clone().instrument_markers(&mut counter),
                Mode::Collapse(winners) => t.clone().collapse_by_index(&mut counter, &winners),
            };
            st.1 = counter;
            out
        })
    }

    /// Parse the winning branch of every instrumented `First` from a `lake
    /// build` log. Markers surface as `… AVERMIN:<idx>:<branch>`; `first` traces
    /// every branch it tries and stops at the first that closes, so the winner
    /// of node `idx` is the MAXIMUM branch index seen for it.
    pub fn parse_winners(build_output: &str) -> BTreeMap<usize, usize> {
        let mut winners: BTreeMap<usize, usize> = BTreeMap::new();
        for line in build_output.lines() {
            let Some(pos) = line.find("AVERMIN:") else {
                continue;
            };
            let rest = &line[pos + "AVERMIN:".len()..];
            let mut it = rest.split(|c: char| !c.is_ascii_digit());
            let (Some(i), Some(b)) = (it.next(), it.next()) else {
                continue;
            };
            let (Ok(idx), Ok(branch)) = (i.parse::<usize>(), b.parse::<usize>()) else {
                continue;
            };
            let e = winners.entry(idx).or_insert(branch);
            *e = (*e).max(branch);
        }
        winners
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn leaf(s: &str) -> Tactic {
        Tactic::Leaf(s.to_string())
    }

    #[test]
    fn renders_flat_portfolio_inline_with_bare_sorry() {
        // The String-length-additivity rung shape.
        let t = Tactic::Seq(vec![
            leaf("intro a b"),
            Tactic::First(vec![
                leaf("simp only [String.add_eq_append, String.length_append] <;> omega"),
                Tactic::Sorry,
            ]),
        ]);
        assert_eq!(
            t.render(),
            vec![
                "intro a b".to_string(),
                "first | (simp only [String.add_eq_append, String.length_append] <;> omega) | sorry"
                    .to_string(),
            ]
        );
    }

    #[test]
    fn renders_grind_wrapped_two_level_portfolio() {
        // The 2-level grind-wrap: `first | (grind…) | (<inner first>)`.
        let inner = Tactic::First(vec![
            leaf("exact AverMap.len_set_ge_one _ _ _"),
            Tactic::Sorry,
        ]);
        let t = Tactic::Seq(vec![
            leaf("intro m k v"),
            Tactic::First(vec![leaf("grind [_root_.after]; done"), inner]),
        ]);
        // Both branches are single-line, so the whole thing is inline.
        assert_eq!(
            t.render(),
            vec![
                "intro m k v".to_string(),
                "first | (grind [_root_.after]; done) | (first | (exact AverMap.len_set_ge_one _ _ _) | sorry)".to_string(),
            ]
        );
    }

    #[test]
    fn renders_induction_arms() {
        let t = Tactic::Seq(vec![
            leaf("intro xs"),
            Tactic::Induction {
                target: "xs".to_string(),
                arms: vec![
                    InductionArm {
                        pattern: "nil".to_string(),
                        body: Tactic::First(vec![leaf("simp [f]"), Tactic::Sorry]),
                    },
                    InductionArm {
                        pattern: "cons h t ih".to_string(),
                        body: leaf("simp_all [f]"),
                    },
                ],
            },
        ]);
        assert_eq!(
            t.render(),
            vec![
                "intro xs".to_string(),
                "induction xs with".to_string(),
                "| nil => first | (simp [f]) | sorry".to_string(),
                "| cons h t ih => simp_all [f]".to_string(),
            ]
        );
    }

    #[test]
    fn first_count_is_preorder_total() {
        let t = Tactic::Seq(vec![
            Tactic::First(vec![leaf("a"), Tactic::Sorry]),
            Tactic::Induction {
                target: "xs".to_string(),
                arms: vec![InductionArm {
                    pattern: "cons h t".to_string(),
                    body: Tactic::First(vec![leaf("b"), leaf("c")]),
                }],
            },
        ]);
        assert_eq!(t.first_count(), 2);
    }

    #[test]
    fn render_body_is_byte_identical_for_baked_raw() {
        // A `raw`-migrated body carries the legacy baked 2-space indent (with a
        // deeper 4-space continuation). `render_body` strips the common 2 and
        // re-adds it at indent 1 — reproducing the exact legacy lines.
        let baked = vec![
            "  intro xs".to_string(),
            "  induction xs with".to_string(),
            "  | nil => simp".to_string(),
            "  | cons h t ih =>".to_string(),
            "    simp [ih]".to_string(),
        ];
        let body = Tactic::raw(baked.clone());
        assert_eq!(body.render_body(), baked);
    }

    #[test]
    fn render_body_lays_out_grind_wrapped_first_at_two_space() {
        // The grind-wrap shape: un-baked intro + `First`, the body branch a
        // baked multi-line `raw`. `render_body` must put `first`/`|` at the
        // 2-space column (matching `intro`) and re-base the body branch under
        // `| (` without flattening it.
        let body = Tactic::Seq(vec![
            leaf("intro a b"),
            Tactic::First(vec![
                leaf("grind [f]; done"),
                Tactic::raw(vec![
                    "  simp [f]".to_string(),
                    "  omega".to_string(),
                    "  sorry".to_string(),
                ]),
            ]),
        ]);
        assert_eq!(
            body.render_body(),
            vec![
                "  intro a b".to_string(),
                "  first".to_string(),
                "  | (grind [f]; done)".to_string(),
                "  | (".to_string(),
                "    simp [f]".to_string(),
                "    omega".to_string(),
                "    sorry".to_string(),
                "  )".to_string(),
            ]
        );
    }

    #[test]
    fn collapse_firsts_picks_the_winning_branch() {
        // Minimize: pick branch 0 of every First — drops the alternation + sorry.
        let t = Tactic::Seq(vec![
            leaf("intro a b"),
            Tactic::First(vec![
                leaf("the_winner <;> omega"),
                leaf("loser"),
                Tactic::Sorry,
            ]),
        ]);
        let mut pick = |_branches: &[Tactic]| Some(0usize);
        let minimized = t.collapse_firsts(&mut pick);
        assert_eq!(
            minimized.render(),
            vec!["intro a b".to_string(), "the_winner <;> omega".to_string()]
        );
    }

    #[test]
    fn parse_winners_takes_max_branch_per_index() {
        // First 0 traced branches 0 then 1 (1 won); First 1 traced only 0
        // (0 won); a duplicate re-elaboration line must not change the max.
        let log = "\
info: F.lean:3:5: AVERMIN:0:0
info: F.lean:4:5: AVERMIN:0:1
info: G.lean:9:5: AVERMIN:1:0
info: F.lean:4:5: AVERMIN:0:1
warning: declaration uses 'sorry'
";
        let w = minimize::parse_winners(log);
        assert_eq!(w.get(&0), Some(&1));
        assert_eq!(w.get(&1), Some(&0));
        assert_eq!(w.len(), 2);
    }

    #[test]
    fn instrument_markers_number_firsts_in_preorder() {
        // Outer First (idx 0) whose branch 1 holds a nested First (idx 1).
        let inner = Tactic::First(vec![leaf("a"), Tactic::Sorry]);
        let t = Tactic::First(vec![leaf("grind; done"), inner]);
        let mut next = 0;
        let instrumented = t.instrument_markers(&mut next);
        assert_eq!(next, 2); // two First nodes numbered
        let rendered = instrumented.render().join("\n");
        // Outer node 0: both branches marked; nested node 1: both branches marked.
        assert!(rendered.contains("AVERMIN:0:0"));
        assert!(rendered.contains("AVERMIN:0:1"));
        assert!(rendered.contains("AVERMIN:1:0"));
        assert!(rendered.contains("AVERMIN:1:1"));
    }

    #[test]
    fn collapse_by_index_keeps_winner_and_stays_index_aligned() {
        // Same shape as the instrument test. Winner of outer (0) is branch 1
        // (the nested First); winner of nested (1) is branch 0 (`a`).
        let inner = Tactic::First(vec![leaf("a"), Tactic::Sorry]);
        let t = Tactic::Seq(vec![
            leaf("intro x"),
            Tactic::First(vec![leaf("grind; done"), inner]),
        ]);
        let winners = BTreeMap::from([(0usize, 1usize), (1usize, 0usize)]);
        let mut next = 0;
        let collapsed = t.collapse_by_index(&mut next, &winners);
        assert_eq!(next, 2); // walked ALL branches, both Firsts counted
        assert_eq!(
            collapsed.render(),
            vec!["intro x".to_string(), "a".to_string()]
        );
    }

    #[test]
    fn render_body_round_trips_instrument_then_collapse() {
        // The grind-wrap shape. Instrument emits markers; suppose the probe
        // build reports the body branch (1) as the winner — collapse drops the
        // grind arm, leaving just the body.
        let grind_wrap = || {
            Tactic::Seq(vec![
                leaf("intro a b"),
                Tactic::First(vec![
                    leaf("grind [f]; done"),
                    Tactic::raw_dedented(vec!["  simp [f]".to_string(), "  sorry".to_string()]),
                ]),
            ])
        };

        minimize::begin_instrument();
        let instrumented = grind_wrap().render_body().join("\n");
        minimize::end();
        assert!(instrumented.contains("AVERMIN:0:0"));
        assert!(instrumented.contains("AVERMIN:0:1"));

        minimize::begin_collapse(BTreeMap::from([(0usize, 1usize)]));
        let collapsed = grind_wrap().render_body();
        minimize::end();
        assert_eq!(
            collapsed,
            vec![
                "  intro a b".to_string(),
                "  simp [f]".to_string(),
                "  sorry".to_string(),
            ]
        );

        // Pass ended → emission is byte-for-byte normal again.
        assert_eq!(
            grind_wrap().render_body(),
            vec![
                "  intro a b".to_string(),
                "  first".to_string(),
                "  | (grind [f]; done)".to_string(),
                "  | (".to_string(),
                "    simp [f]".to_string(),
                "    sorry".to_string(),
                "  )".to_string(),
            ]
        );
    }
}
