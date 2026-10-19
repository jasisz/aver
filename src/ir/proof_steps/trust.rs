//! What a step proof stands on, read off the script the kernel checks: the
//! wall rules its steps apply, whether it uses a `compute`, `ring` or
//! `linear` step (an evaluator or a decision procedure of the kernel rather
//! than a rule), and the earlier laws and builtin facts it cites. The proofs
//! of the cited facts travel in the script and are walked too; a cited law
//! stands on its own script, which has its own entry.
//!
//! The text is read with [`super::read::parse`] and walked by the grammar
//! of [`super::sexpr`], so only proof positions count: a term or a name
//! that happens to be spelled `rule` is never read as a step.

use std::collections::BTreeSet;

use super::read::{Sx, parse};

/// The trust base of one script.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Trust {
    /// Wall rule identifiers ([`super::WallRule::id`]) the steps apply.
    pub rules: BTreeSet<String>,
    pub compute: bool,
    pub ring: bool,
    pub linear: bool,
    /// Earlier laws the steps cite, taken as given.
    pub laws: BTreeSet<String>,
    /// Builtin facts the steps cite, proved inside the script.
    pub facts: BTreeSet<String>,
}

impl Trust {
    /// The entry `aver proof --check-json` writes under `trust`.
    pub fn to_json(&self) -> serde_json::Value {
        let names = |s: &BTreeSet<String>| {
            serde_json::Value::Array(s.iter().map(|n| n.clone().into()).collect())
        };
        serde_json::json!({
            "rules": names(&self.rules),
            "compute": self.compute,
            "ring": self.ring,
            "linear": self.linear,
            "laws": names(&self.laws),
            "facts": names(&self.facts),
        })
    }
}

/// The trust base of a step script, from its text.
pub fn of_script(text: &str) -> Result<Trust, String> {
    let sx = super::shared::expand(parse(text)?)?;
    let Sx::List(parts) = &sx else {
        return Err("not a step script".into());
    };
    let section = |name: &str| -> Option<&[Sx]> {
        parts.iter().find_map(|p| match p {
            Sx::List(xs) if matches!(xs.first(), Some(Sx::Atom(a)) if a == name) => Some(&xs[1..]),
            _ => None,
        })
    };
    let mut walk = Walk::default();
    let mut fact_keys = BTreeSet::new();
    for cited in section("laws").unwrap_or(&[]) {
        if let Sx::List(xs) = cited
            && let [Sx::Atom(head), Sx::Atom(key), .., last] = xs.as_slice()
            && head == "fact"
        {
            fact_keys.insert(key.clone());
            walk.proof(last)?;
        }
    }
    match section("proof") {
        Some([p]) => walk.proof(p)?,
        _ => return Err("a script without one proof".into()),
    }
    let (facts, laws) = walk.cited.into_iter().partition(|k| fact_keys.contains(k));
    Ok(Trust {
        rules: walk.rules,
        compute: walk.compute,
        ring: walk.ring,
        linear: walk.linear,
        laws,
        facts,
    })
}

#[derive(Default)]
struct Walk {
    rules: BTreeSet<String>,
    cited: BTreeSet<String>,
    compute: bool,
    ring: bool,
    linear: bool,
}

fn items(s: &Sx) -> Result<&[Sx], String> {
    match s {
        Sx::List(xs) => Ok(xs),
        _ => Err("expected a list".into()),
    }
}

impl Walk {
    fn proofs(&mut self, ps: Option<&[Sx]>) -> Result<(), String> {
        for p in ps.unwrap_or(&[]) {
            self.proof(p)?;
        }
        Ok(())
    }

    fn proof(&mut self, s: &Sx) -> Result<(), String> {
        let xs = items(s)?;
        let Some(Sx::Atom(tag)) = xs.first() else {
            return Err("expected a step".into());
        };
        let args = &xs[1..];
        match tag.as_str() {
            "refl" | "const" | "proj" | "cell" | "hyp" => Ok(()),
            "compute" => {
                self.compute = true;
                Ok(())
            }
            "ring" => {
                self.ring = true;
                Ok(())
            }
            "linear" => {
                self.linear = true;
                Ok(())
            }
            "symm" | "absurd" => self.proofs(args.get(..1)),
            "trans" | "congr" => self.proofs(args.get(1..)),
            "cases" | "have" => self.proofs(args.get(2..)),
            "enum" => self.proofs(args.get(3..)),
            "arm" => self.proofs(args.get(3..)),
            "unfold" => self.proofs(args.get(4..)),
            "rule" | "law" => {
                let Some(Sx::Atom(id)) = args.first() else {
                    return Err(format!("malformed {tag}"));
                };
                if tag == "rule" {
                    self.rules.insert(id.clone());
                } else {
                    self.cited.insert(id.clone());
                }
                self.proofs(args.get(2..))
            }
            // (split TERM NAME (LITERAL…) (case CTOR (NAME…) PROOF)…)
            "split" => {
                for c in args.get(3..).unwrap_or(&[]) {
                    self.proofs(items(c)?.get(3..))?;
                }
                Ok(())
            }
            // (induct FN (TERM…) TERM TERM [(carry NAME…)] (case (NAME…) (IH…) (MORE…) PROOF)…)
            "induct" => {
                for c in carried(args.get(4..).unwrap_or(&[])) {
                    let c = items(c)?;
                    for ih in items(c.get(2).ok_or("malformed case")?)? {
                        if let Sx::List(ps) = ih {
                            self.proofs(ps.get(1..))?;
                        }
                    }
                    for more in items(c.get(3).ok_or("malformed case")?)? {
                        self.proofs(Some(items(items(more)?.get(3).ok_or("malformed case")?)?))?;
                    }
                    self.proofs(c.get(4..))?;
                }
                Ok(())
            }
            // (listinduct NAME TERM TERM [(carry NAME…)] PROOF (HEAD TAIL) (NAME…) ((IH (TERM…) (PROOF…))…) PROOF)
            "listinduct" => {
                let rest = carried(args.get(3..).unwrap_or(&[]));
                let [nil, _, _, ihs, cons] = rest else {
                    return Err("malformed listinduct".into());
                };
                self.proof(nil)?;
                for ih in items(ihs)? {
                    self.proofs(Some(items(
                        items(ih)?.get(2).ok_or("malformed listinduct")?,
                    )?))?;
                }
                self.proof(cons)
            }
            // (listcases NAME TERM TERM PROOF (HEAD TAIL) PROOF)
            "listcases" => match args {
                [_, _, _, nil, _, cons] => {
                    self.proof(nil)?;
                    self.proof(cons)
                }
                _ => Err("malformed listcases".into()),
            },
            other => Err(format!("unknown step {other}")),
        }
    }
}

/// The forms after an optional leading `(carry NAME…)`.
fn carried(xs: &[Sx]) -> &[Sx] {
    match xs.first() {
        Some(Sx::List(c)) if matches!(c.first(), Some(Sx::Atom(a)) if a == "carry") => &xs[1..],
        _ => xs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_rules_procedures_and_citations_from_proof_positions_only() {
        let text = "(steps 16 (obligation k ((rule int)) (none) (v rule) (v rule)) (defs) (consts) (sums) (recs) (sigs) \
            (laws (law L ((x int)) (none) (v x) (v x)) (fact F ((x int)) (none) (v x) (v x) (rule bool.and.true_r ((a (v x)))))) \
            (proof (trans ((v rule) (v rule) (v rule)) (law L ((x (v rule)))) (cases (v c) h (law F ((x (v rule)))) \
            (listcases xs (v xs) (v xs) (compute (i 1) (i 1)) (h t) (induct f ((v n)) (v n) (v n) (carry when) \
            (case (g) ((ih (linear (v g) true () (1)))) ((0 ih2 () ((ring (i 1) (i 1))))) (rule bits.not ((a (v n)))))))))))";
        let t = of_script(text).unwrap();
        assert_eq!(
            t.rules.into_iter().collect::<Vec<_>>(),
            ["bits.not", "bool.and.true_r"]
        );
        assert!(t.compute && t.ring && t.linear);
        assert_eq!(t.laws.into_iter().collect::<Vec<_>>(), ["L"]);
        assert_eq!(t.facts.into_iter().collect::<Vec<_>>(), ["F"]);
    }

    #[test]
    fn a_proof_by_reflexivity_stands_on_nothing() {
        let text = "(steps 16 (obligation k () (none) (i 1) (i 1)) (defs) (consts) (sums) (recs) (sigs) (laws) (proof (refl (i 1))))";
        assert_eq!(of_script(text).unwrap(), Trust::default());
    }
}
