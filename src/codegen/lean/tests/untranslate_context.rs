use super::*;
use crate::codegen::lean::untranslate::{context_for_law, untranslate_premise_json};

fn law_with_type(ty: &str, sample: &str) -> VerifyLaw {
    let source = format!(
        "fn f(x: {ty}) -> Bool\n    true\nverify f law identity\n    given x: {ty} = [{sample}]\n    f(x) holds\n"
    );
    parse_source(&source)
        .unwrap()
        .into_iter()
        .find_map(|item| {
            if let TopLevel::Verify(block) = item
                && let VerifyKind::Law(law) = block.kind
            {
                Some(*law)
            } else {
                None
            }
        })
        .unwrap()
}

fn projection(owner: &str, index: usize) -> serde_json::Value {
    serde_json::json!({"app": {"fn": {"const": "LE.le"}, "args": [
        {"const": "Int"}, {"const": "Int.instLEInt"}, {"nat": "0"},
        {"proj": {"struct": owner, "idx": index, "e": {"var": "x"}}}
    ]}})
}

#[test]
fn untranslate_record_context_uses_exact_owner_and_declared_field_order() {
    let mut ctx = empty_ctx();
    ctx.items =
        parse_source("module Entry\n    effects []\nrecord Shared\n    local: Int\n").unwrap();
    ctx.type_defs = ctx
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::TypeDef(td) = item {
                Some(td.clone())
            } else {
                None
            }
        })
        .collect();
    for (owner, field) in [("Left", "left"), ("Right", "right")] {
        let source = format!(
            "module {owner}\n    effects []\nrecord Shared\n    first: Int\n    {field}: Int\n"
        );
        ctx.modules.push(crate::codegen::ModuleInfo::from_items(
            owner.to_string(),
            &parse_source(&source).unwrap(),
            None,
        ));
    }
    ctx.symbol_table = crate::ir::SymbolTable::build(&ctx.items, &ctx.modules);
    let display = context_for_law(&ctx, None, &law_with_type("Shared", "Shared(local = 0)"));
    for (owner, index, expected) in [
        ("Entry.Shared", 0, "0 <= x.local"),
        ("Left.Shared", 1, "0 <= x.left"),
        ("Right.Shared", 1, "0 <= x.right"),
    ] {
        let premise =
            untranslate_premise_json(&projection(owner, index).to_string(), &display).unwrap();
        assert_eq!(crate::checker::expr_to_str(&premise.expression), expected);
        let mut accessor = projection(owner, index);
        let field = &display.record_fields[owner][index];
        accessor["app"]["args"][3] = serde_json::json!({"app": {
            "fn": {"const": format!("{owner}.{}", crate::codegen::lean::aver_name_to_lean(field))},
            "args": [{"var": "x"}]
        }});
        let premise = untranslate_premise_json(&accessor.to_string(), &display).unwrap();
        assert_eq!(crate::checker::expr_to_str(&premise.expression), expected);
    }
    for (owner, index) in [
        ("Shared", 0),
        ("Unknown.Shared", 0),
        ("Left.Shared", 2),
        ("Subtype", 0),
    ] {
        assert!(untranslate_premise_json(&projection(owner, index).to_string(), &display).is_err());
    }
    assert!(
        untranslate_premise_json(
            &projection("Left.Shared", 1).to_string(),
            &Default::default()
        )
        .is_err()
    );
}

#[test]
fn untranslate_record_context_declines_refinement_structure_lifts() {
    let ctx = ctx_from_source(
        r#"
module Carrier
    effects []
record Natural
    value: Int
fn fromInt(n: Int) -> Result<Natural, String>
    match n >= 0
        true -> Result.Ok(Natural(value = n))
        false -> Result.Err("negative")
record Held
    payload: Natural
    line: Int
"#,
        "Carrier",
    );
    let display = context_for_law(&ctx, None, &law_with_type("Natural", "Natural(value = 0)"));
    assert!(!display.record_fields.contains_key("Carrier.Natural"));
    assert_eq!(display.record_fields["Carrier.Held"], ["payload", "line"]);
}

#[test]
fn untranslate_record_context_resolves_imported_peano_by_law_owner() {
    let mut ctx = empty_ctx();
    for (owner, zero, succ) in [("Left", "Zero", "Next"), ("Right", "Nil", "More")] {
        let source =
            format!("module {owner}\n    effects []\ntype Count\n    {zero}\n    {succ}(Count)\n");
        ctx.modules.push(crate::codegen::ModuleInfo::from_items(
            owner.to_string(),
            &parse_source(&source).unwrap(),
            None,
        ));
    }
    ctx.symbol_table = crate::ir::SymbolTable::build(&ctx.items, &ctx.modules);
    for (owner, zero) in [("Left", "Zero"), ("Right", "Nil")] {
        let display = context_for_law(
            &ctx,
            Some(owner),
            &law_with_type("Count", &format!("Count.{zero}")),
        );
        let peano = display.peano.unwrap();
        assert_eq!(peano.type_name, format!("{owner}.Count"));
        assert_eq!(peano.zero_ctor, zero);
    }
}
