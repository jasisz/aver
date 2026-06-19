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
        let strip = self.leaf_min_indent().unwrap_or(0);
        self.clone().strip_leaf_indent(strip).render_indent(1)
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
}
