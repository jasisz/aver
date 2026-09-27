use super::*;
use crate::codegen::lean::{isolate, sample_literal};

const INTERLEAVED: &str = r#"module Identity
    effects [Random.int, Disk.readText]

fn low(path: BranchPath, call: Int, min: Int, max: Int) -> Result<Int, String>
    Result.Ok(min)

fn high(path: BranchPath, call: Int, min: Int, max: Int) -> Result<Int, String>
    Result.Ok(max)

fn pick(go: Bool) -> Int
    ! [Disk.readText]
    1

verify pick
    given rnd: Random.int = [low]
    pick(false) => 1

verify pick
    given rnd: Random.int = [high]
    pick(false) => 2

verify pick
    given rnd: Random.int = [low]
    pick(false) => 1
"#;

fn blocks(ctx: &CodegenContext) -> Vec<VerifyBlock> {
    ctx.items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Verify(vb) => Some(vb.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn source_identity_routes_vm_results_and_keeps_theorem_names_in_source_order() {
    let mut ctx = ctx_from_source(INTERLEAVED, "Identity");
    let source_blocks = blocks(&ctx);
    let ids: Vec<_> = source_blocks
        .iter()
        .map(|b| b.source_case_id(0).unwrap())
        .collect();
    let merged = crate::checker::merge_verify_blocks(&ctx.items);
    assert_eq!(merged.len(), 2);
    assert_eq!(merged[0].case_ids, vec![Some(ids[0]), Some(ids[2])]);
    assert_eq!(merged[1].case_ids, vec![Some(ids[1])]);

    let results = crate::diagnostics::vm_verify::run_verify_for_items_vm(
        ctx.items.clone(),
        None,
        None,
        "identity.av",
    )
    .expect("VM verification");
    let mut passed = Vec::new();
    let mut failed = Vec::new();
    for case in results.iter().flat_map(|r| &r.case_results) {
        let id = case.case_id.expect("VM preserves source identity");
        match case.outcome {
            crate::checker::VerifyCaseOutcome::Pass => {
                passed.push(id);
                for scope in [None, Some("Identity".to_string())] {
                    ctx.vm_passed_cases.insert((scope.clone(), id));
                    ctx.sample_expected.insert((scope, id), "1".to_string());
                }
            }
            crate::checker::VerifyCaseOutcome::Mismatch { .. } => failed.push(id),
            ref outcome => panic!("unexpected outcome: {outcome:?}"),
        }
    }
    assert_eq!(passed, vec![ids[0], ids[2]]);
    assert_eq!(failed, vec![ids[1]]);
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    for index in [1, 3] {
        assert_eq!(
            lean.matches(&format!("theorem __aver_verify_pick_{index} "))
                .count(),
            1,
            "{lean}"
        );
    }
    assert!(!lean.contains("__aver_verify_pick_2"), "{lean}");
    assert!(
        lean.lines()
            .any(|line| line.starts_with("example (rnd_Disk_readText")
                && line.contains("= (2 : Int)")),
        "{lean}"
    );

    // A decline for the third source case must never suppress the false second.
    let third = ids[2];
    for scope in [None, Some("Identity".to_string())] {
        ctx.declined_cases
            .insert((scope, third), "case budget".to_string());
    }
    assert!(sample_literal::decline_reason(&source_blocks[1], &ctx, 0).is_none());
    assert_eq!(
        sample_literal::decline_reason(&source_blocks[2], &ctx, 0).map(String::as_str),
        Some("case budget")
    );
}

#[test]
fn absent_source_identity_keeps_the_source_equation_outside_isolation() {
    let mut ctx = ctx_from_source(INTERLEAVED, "Identity");
    let original = blocks(&ctx);
    for block in &original {
        let id = block.source_case_id(0).unwrap();
        for scope in [None, Some("Identity".to_string())] {
            ctx.vm_passed_cases.insert((scope.clone(), id));
            ctx.sample_expected
                .insert((scope.clone(), id), "99".to_string());
            ctx.declined_cases
                .insert((scope, id), "wrong provenance".to_string());
        }
    }
    for item in &mut ctx.items {
        if let TopLevel::Verify(vb) = item {
            vb.case_ids.clear();
        }
    }
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(!lean.contains(isolate::ISOLATION_GUARD), "{lean}");
    assert_eq!(
        lean.lines()
            .filter(|l| l.starts_with("example (rnd_Disk_readText"))
            .count(),
        3,
        "{lean}"
    );
    assert!(lean.contains("= (2 : Int)"), "{lean}");
    assert!(!lean.contains("= (99 : Int)"), "{lean}");
    assert!(ctx.declined_claims.borrow().is_empty());
}

#[test]
fn expanded_samples_and_explanations_have_distinct_source_identities() {
    let ctx = ctx_from_source(
        r#"module Samples
fn same(n: Int) -> Int
    n

verify same law identity
    given n: Int = [1, 2]
    same(n) => n
"#,
        "Samples",
    );
    let mut vb = blocks(&ctx).remove(0);
    assert_eq!(vb.cases.len(), 2);
    assert_ne!(vb.source_case_id(0), vb.source_case_id(1));
    let VerifyKind::Law(law) = &mut vb.kind else {
        panic!("law")
    };
    law.because = vec![Spanned::bare(Expr::Literal(Literal::Bool(true)))];
    let explanation = crate::verify_law::reasons::sample_blocks(&vb).remove(0);
    let identities: std::collections::HashSet<_> = (0..2)
        .flat_map(|i| {
            [
                vb.source_case_id(i).unwrap(),
                explanation.source_case_id(i).unwrap(),
            ]
        })
        .collect();
    assert_eq!(identities.len(), 4);
}
