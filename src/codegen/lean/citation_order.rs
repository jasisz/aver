//! Citation order of the verify blocks a Lean proof export emits.
//!
//! A law theorem may cite, as lemmas, only theorems declared ABOVE it in the
//! Lean file, and every "earlier laws" pool in the strategies walks the file's
//! verify blocks up to the law's own block. Emitting blocks in SOURCE order
//! made the formatter decide what a law can cite: `aver format` moves every
//! `verify f law …` block right behind `verify f`, so a round trip on `digits`
//! that needs the snoc law on `bigEndian` lost that lemma whenever `digits`
//! was defined first. This pass orders the blocks the way a reader would
//! have to write them by hand: a block about a function comes before every
//! law whose call cone reaches that function (its laws are the candidate
//! lemmas), ties and cycles keep source order. Definitions are emitted ahead
//! of every theorem, so moving a block never moves a theorem above a def it
//! mentions. Idempotent: ordering an ordered context changes nothing.
use std::collections::BTreeSet;

use crate::ast::{TopLevel, VerifyBlock, VerifyKind};
use crate::codegen::CodegenContext;
use crate::codegen::proof_lower::{LawProofCone, ProofLowerInputs};

/// A citation the order cannot honour: two law blocks whose cones reach each
/// other's function, so whichever is written first cannot cite the other.
/// Reported as a `warning[proof-citation-cycle]` by `aver proof`; the pair
/// keeps source order, which is what the export always did.
pub struct CitationCycle {
    pub line: usize,
    pub message: String,
}

/// Reorder the entry module's verify items and every dependency module's
/// verify block lists into citation order (see the module doc). Returns the
/// cycles the order had to break.
pub fn order_verify_blocks_for_citation(ctx: &mut CodegenContext) -> Vec<CitationCycle> {
    let mut cycles = Vec::new();
    let entry_order = {
        let blocks: Vec<&VerifyBlock> = ctx
            .items
            .iter()
            .filter_map(|item| match item {
                TopLevel::Verify(vb) => Some(vb),
                _ => None,
            })
            .collect();
        let (order, broken) = citation_order(&blocks, ctx);
        cycles.extend(broken);
        order
    };
    let mut reordered: Vec<VerifyBlock> = {
        let blocks: Vec<&VerifyBlock> = ctx
            .items
            .iter()
            .filter_map(|item| match item {
                TopLevel::Verify(vb) => Some(vb),
                _ => None,
            })
            .collect();
        entry_order.iter().map(|&i| blocks[i].clone()).collect()
    };
    reordered.reverse();
    for item in &mut ctx.items {
        if let TopLevel::Verify(vb) = item
            && let Some(next) = reordered.pop()
        {
            *vb = next;
        }
    }
    for i in 0..ctx.modules.len() {
        let prefix = ctx.modules[i].prefix.clone();
        let (order, broken) = ctx.with_module_scope(Some(prefix.as_str()), || {
            let blocks: Vec<&VerifyBlock> = ctx.modules[i].verify_blocks.iter().collect();
            citation_order(&blocks, ctx)
        });
        cycles.extend(broken);
        let module = &mut ctx.modules[i];
        let blocks: Vec<VerifyBlock> = order
            .iter()
            .map(|&k| module.verify_blocks[k].clone())
            .collect();
        module.verify_blocks = blocks;
        let position = |vb: &VerifyBlock| {
            module
                .verify_blocks
                .iter()
                .position(|b| b.line == vb.line && b.fn_name == vb.fn_name)
                .unwrap_or(usize::MAX)
        };
        module.verify_laws.sort_by_key(position);
    }
    cycles
}

/// The permutation (indices into `blocks`) that puts a block about a function
/// before every law block whose cone reaches that function. Kahn's algorithm
/// taking the smallest source index available at each step, so blocks with no
/// constraint between them keep source order; a cycle (two laws each citing
/// the other's function) is broken at its smallest source index, i.e. kept in
/// source order — exactly what the export did before this pass — and reported,
/// naming the block that goes first and the one that can therefore not be
/// cited by it.
fn citation_order(
    blocks: &[&VerifyBlock],
    ctx: &CodegenContext,
) -> (Vec<usize>, Vec<CitationCycle>) {
    let inputs = ProofLowerInputs::from_ctx(ctx);
    let reaches: Vec<BTreeSet<String>> = blocks
        .iter()
        .map(|vb| match &vb.kind {
            VerifyKind::Law(law) => LawProofCone::compute(law, &vb.fn_name, &inputs)
                .pure_fns()
                .iter()
                .map(|fd| fd.name.clone())
                .collect(),
            VerifyKind::Cases => BTreeSet::new(),
        })
        .collect();
    let n = blocks.len();
    let mut indegree = vec![0usize; n];
    let mut successors: Vec<Vec<usize>> = vec![Vec::new(); n];
    // Explicit citations constrain emission before inferred cone preferences.
    // An inferred reverse edge must not move a requested supplier below its
    // consumer. Modules without explicit citations retain their old ordering.
    let has_explicit = blocks
        .iter()
        .any(|b| matches!(&b.kind, VerifyKind::Law(l) if l.using.is_some()));
    for consumer in 0..n {
        for cited in 0..n {
            let explicit = match &blocks[consumer].kind {
                VerifyKind::Law(law) => law
                    .using
                    .as_ref()
                    .map(|names| names.contains(&law_label(blocks[cited]))),
                VerifyKind::Cases => None,
            };
            if consumer != cited && explicit == Some(true) {
                successors[cited].push(consumer);
                indegree[consumer] += 1;
            }
        }
    }
    for consumer in 0..n {
        if matches!(&blocks[consumer].kind, VerifyKind::Law(l) if l.using.is_some()) {
            continue;
        }
        for cited in 0..n {
            if consumer == cited
                || blocks[cited].fn_name == blocks[consumer].fn_name
                || !reaches[consumer].contains(&blocks[cited].fn_name)
            {
                continue;
            }
            if has_explicit {
                let mut pending = vec![consumer];
                let mut visited = vec![false; n];
                while let Some(next) = pending.pop() {
                    if !visited[next] {
                        visited[next] = true;
                        pending.extend(successors[next].iter().copied());
                    }
                }
                if visited[cited] {
                    continue;
                }
            }
            successors[cited].push(consumer);
            indegree[consumer] += 1;
        }
    }
    let mut placed = vec![false; n];
    let mut order = Vec::with_capacity(n);
    let mut cycles = Vec::new();
    while order.len() < n {
        let free = (0..n).find(|&i| !placed[i] && indegree[i] == 0);
        let next = free.or_else(|| (0..n).find(|&i| !placed[i]));
        let Some(i) = next else {
            break;
        };
        if free.is_none() {
            // Every unplaced block still waits on another: `i` goes first by
            // source order, and the unplaced blocks it would have cited are
            // the ones it can no longer cite.
            let waiting: Vec<String> = (0..n)
                .filter(|&j| !placed[j] && j != i && successors[j].contains(&i))
                .map(|j| format!("`{}` (line {})", law_label(blocks[j]), blocks[j].line))
                .collect();
            if !waiting.is_empty() {
                cycles.push(CitationCycle {
                    line: blocks[i].line,
                    message: format!(
                        "law `{}` and {} reach each other's functions; `{}` is written \
                         first and so cannot cite {} — reorder the blocks if it needs to",
                        law_label(blocks[i]),
                        waiting.join(", "),
                        law_label(blocks[i]),
                        if waiting.len() == 1 { "it" } else { "them" }
                    ),
                });
            }
        }
        placed[i] = true;
        order.push(i);
        for &s in &successors[i] {
            if !placed[s] {
                indegree[s] = indegree[s].saturating_sub(1);
            }
        }
    }
    (order, cycles)
}

fn law_label(vb: &VerifyBlock) -> String {
    match &vb.kind {
        VerifyKind::Law(law) => format!("{}.{}", vb.fn_name, law.name),
        VerifyKind::Cases => vb.fn_name.clone(),
    }
}
