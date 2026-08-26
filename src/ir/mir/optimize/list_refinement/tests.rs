use super::*;
use crate::ir::mir::{MirCallee, lower_program};
use crate::source::parse_source;

const PROGRAM: &str = r#"
module Octets
    intent = "exercise identity-pinned list refinement provenance"
    effects []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn octets(value: Octets) -> List<Int>
    value.values

fn direct(value: Octets) -> Result<Octets, String>
    fromList(octets(value))

fn combined(left: Octets, right: Octets) -> Result<Octets, String>
    fromList(List.concat(octets(left), octets(right)))

fn sliced(value: Octets) -> Result<Octets, String>
    kept = List.take(octets(value), 4)
    fromList(List.drop(kept, 1))

fn widened(value: Octets, arbitrary: Int) -> Result<Octets, String>
    fromList(List.prepend(arbitrary, octets(value)))
"#;

fn optimized() -> (MirProgram, crate::ir::SymbolTable) {
    let mut items = parse_source(PROGRAM).expect("parse");
    let result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    assert!(
        result
            .typecheck
            .as_ref()
            .is_none_or(|tc| tc.errors.is_empty()),
        "type errors: {:?}",
        result.typecheck.as_ref().map(|tc| &tc.errors)
    );
    let program = super::super::optimize_with_list_refinements(
        lower_program(&result.resolved_items),
        result.symbol_table.literal_refinements(),
    );
    (program, result.symbol_table)
}

fn calls_fn(expr: &MirExpr, target: FnId) -> bool {
    if matches!(
        expr,
        MirExpr::Call(call) if call.node.callee == MirCallee::Fn(target)
    ) {
        return true;
    }
    let mut found = false;
    super::super::super::expr::walk_children(expr, &mut |child| {
        found |= calls_fn(child, target);
    });
    found
}

#[test]
fn discharges_direct_concat_take_and_drop_but_not_prepend() {
    let (program, symbols) = optimized();
    let constructor = symbols
        .fn_id_of(&crate::ir::FnKey::entry("fromList"))
        .expect("constructor identity");
    for name in ["direct", "combined", "sliced"] {
        let id = symbols
            .fn_id_of(&crate::ir::FnKey::entry(name))
            .unwrap_or_else(|| panic!("{name} identity"));
        assert!(
            !calls_fn(
                &program.fn_by_id(id).expect("MIR fn").body.node,
                constructor
            ),
            "{name} retained the unreachable validation call"
        );
    }
    let widened = symbols
        .fn_id_of(&crate::ir::FnKey::entry("widened"))
        .expect("widened identity");
    assert!(
        calls_fn(
            &program.fn_by_id(widened).expect("MIR fn").body.node,
            constructor
        ),
        "List.prepend must invalidate provenance"
    );
}

#[test]
fn a_direct_round_trip_reuses_the_nominal_without_projecting_it() {
    let (program, symbols) = optimized();
    let direct = symbols
        .fn_id_of(&crate::ir::FnKey::entry("direct"))
        .expect("direct identity");
    let MirExpr::Construct(ok) = &program.fn_by_id(direct).expect("MIR fn").body.node else {
        panic!("direct round-trip should be a Result.Ok construct");
    };
    assert_eq!(ok.node.ctor, MirCtor::Builtin(BuiltinCtor::ResultOk));
    assert!(matches!(
        ok.node.args.as_slice(),
        [Spanned {
            node: MirExpr::Local(_),
            ..
        }]
    ));
}
