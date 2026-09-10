use super::*;

#[test]
fn law_function_cone_tracks_cycles_guards_and_reasons_but_not_samples() {
    let source = include_str!("../fixtures/source_recursion/native_sequence.av")
        .replace("fn shuffle(", "fn sample() -> List<Int>\n    [4, 4, 4]\n\nfn guard(items: List<Int>) -> Bool\n    List.len(items) >= 3\n\nfn reason(items: List<Int>) -> Bool\n    true\n\nfn shuffle(")
        .replace("[[4, 4, 4], [9, 9, 9, 9]]", "[sample()]")
        .replace("when List.len(items) >= 3", "when guard(items)\n    because reason(items)");
    let ctx = build_ctx(&source);
    let law = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "threeTurns")
        .unwrap();
    let names: std::collections::BTreeSet<_> = law
        .function_cone
        .iter()
        .map(|id| ctx.symbol_table.fn_entry(*id).key.name.as_str())
        .collect();
    assert_eq!(
        names,
        ["shuffle", "at", "select", "guard", "reason"]
            .into_iter()
            .collect()
    );
    assert_eq!(
        law.function_cone.len(),
        names.len(),
        "Cycles terminate without duplicate declarations"
    );
}
