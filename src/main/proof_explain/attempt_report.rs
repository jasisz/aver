//! Present observed citation attempts separately from source requirements.

use serde_json::{Value, json};

pub(super) fn finish(report: &mut Value) {
    let Some(attempts) = report["citation_attempts"].as_array() else {
        return;
    };
    let matched: Vec<_> = attempts
        .iter()
        .filter(|a| a["outcome"] == "matched")
        .collect();
    if matched.is_empty() {
        return;
    }
    let established: Vec<_> = matched
        .iter()
        .filter(|a| a["established_dependencies"] == true)
        .collect();
    let next = if established.is_empty() {
        "The citation probe has unproved dependencies or an unaudited closure. Resolve failed, bounded, or unchecked dependencies before relying on its closed premises."
    } else if established.iter().any(|a| {
        a["premises"]
            .as_array()
            .is_some_and(|ps| ps.iter().all(|p| p["status"] == "closed"))
    }) {
        "An isolated direct application closed all its premises, while the counted proof remains open. Report a proof-strategy gap at this source step."
    } else {
        "The isolated citation check left open premises. Establish them in earlier because steps or a helper law; the probe does not prove that those premises are false."
    };
    report["next"] = json!(next);
}

pub(super) fn render(report: &Value) {
    if report["citation_probe"]["status"] == "unavailable" {
        println!("  Citation check unavailable; technical details are in proof_citations.log.");
    }
    let Some(attempts) = report["citation_attempts"].as_array() else {
        return;
    };
    for attempt in attempts {
        let outcome = match attempt["outcome"].as_str() {
            Some("matched") => "direct application succeeded",
            Some("application_failed") => "direct application unsuccessful",
            Some("preparation_failed") => "normalization did not complete",
            _ => "diagnostic attempt did not complete",
        };
        println!(
            "  Citation check (isolated): {} — {outcome}",
            attempt["law"].as_str().unwrap_or("")
        );
        if attempt["established_dependencies"] != true {
            println!(
                "    Unproved dependencies or unaudited closure; closed premises are conditional."
            );
            if let Some(laws) = attempt["available_laws"].as_array() {
                for law in laws.iter().filter(|law| law["status"] != "universal") {
                    println!(
                        "    available {} [{}]",
                        law["law"]
                            .as_str()
                            .unwrap_or("<source citation unavailable>"),
                        law["status"].as_str().unwrap_or("not_checked")
                    );
                }
            }
        }
        if attempt["outcome"] == "matched"
            && let Some(premises) = attempt["premises"].as_array()
        {
            for premise in premises {
                println!(
                    "    [{} in probe] {}",
                    premise["status"].as_str().unwrap_or("open"),
                    premise["expression"]
                        .as_str()
                        .unwrap_or("<this premise cannot yet be displayed in Aver>")
                );
                if premise["status"] == "closed" && premise["closure_audited"] != true {
                    println!("      This closure has not passed the proof-axiom audit.");
                }
            }
            if premises.is_empty() {
                println!("    No remaining premises in this isolated application.");
            }
        }
    }
}
