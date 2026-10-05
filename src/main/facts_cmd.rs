//! `aver facts [prefix]`: the builtin facts a law may cite in `using`, each
//! with its statement written as an Aver law.

use aver::ir::proof_steps::facts;

pub(crate) fn cmd_facts(prefix: Option<&str>, json: bool, markdown: bool) {
    let prefix = prefix.unwrap_or("");
    if markdown {
        print!("{}", facts::markdown(prefix));
        return;
    }
    let chosen: Vec<facts::Fact> = facts::all()
        .into_iter()
        .filter(|f| f.key.starts_with(prefix))
        .collect();
    if json {
        let list: Vec<serde_json::Value> = chosen
            .iter()
            .map(|f| {
                serde_json::json!({
                    "name": f.key,
                    "givens": f.script.obligation.givens,
                    "claim": f.claim(),
                    "cites": f.cites(),
                })
            })
            .collect();
        println!("{}", serde_json::Value::Array(list));
        return;
    }
    if chosen.is_empty() {
        println!("no builtin fact starts with `{prefix}`");
        return;
    }
    for f in &chosen {
        println!("{}", f.key);
        println!("    given {}", f.script.obligation.givens.join(", "));
        println!("    {}", f.claim());
    }
    println!("cite one in a law with `using [{}]`", chosen[0].key);
}
