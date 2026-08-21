use super::*;

/// Export `source` to Dafny (no `dafny` binary needed) and return every
/// emitted `.dfy` file concatenated.
fn export_dafny_inline(source: &str, prefix: &str) -> String {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir(&format!("{prefix}-src"));
    std::fs::create_dir_all(&src).expect("create src dir");
    std::fs::write(src.join("m.av"), source).expect("write m.av");
    let out = temp_output_dir(&format!("{prefix}-out"));
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join("m.av"))
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof --backend dafny` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let mut text = String::new();
    for entry in std::fs::read_dir(&out).expect("read out dir") {
        let path = entry.expect("dir entry").path();
        if path.extension().is_some_and(|e| e == "dfy") {
            text.push_str(&std::fs::read_to_string(&path).expect("read .dfy"));
        }
    }
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
    text
}

const MUTUAL_PAIR: &str = "fn pingA(n: Int) -> Int\n    ? \"Bounces to its peer.\"\n    match n <= 0\n        true -> 0\n        false -> pongB(n - 1)\n\n\
fn pongB(n: Int) -> Int\n    ? \"Bounces back.\"\n    match n <= 0\n        true -> 1\n        false -> pingA(n - 1)\n\n";

fn program(module: &str, wrapper: &str) -> String {
    format!(
        "module {module}\n    intent =\n        \"A wrapper reaching a fuel-encoded mutual pair.\"\n\n{MUTUAL_PAIR}{wrapper}"
    )
}

/// The wrapper's law lemma must carry the law's `given` domain as a
/// `requires` and a `{:fuel ...}` attribute for the opaque callee it reaches.
/// Without them the exporter states an unbounded universal it has no basis
/// for (`wrapInterp(3) == "1"`, not `"0"`).
fn assert_lemma_is_bounded_and_fuelled(text: &str, lemma: &str) {
    let sig = format!("{lemma}(n: int)");
    let at = text
        .find(&sig)
        .unwrap_or_else(|| panic!("no lemma `{lemma}` in emitted Dafny:\n{text}"));
    let header_start = text[..at].rfind('\n').map_or(0, |i| i + 1);
    let body_open = text[at..].find("\n{").map_or(text.len(), |i| at + i);
    let lemma_text = &text[header_start..body_open];
    assert!(
        lemma_text.contains("{:fuel pingA"),
        "lemma `{lemma}` lost the fuel attribute for the opaque callee:\n{lemma_text}"
    );
    assert!(
        lemma_text.contains("requires n == 2"),
        "lemma `{lemma}` lost the law's given-domain bound:\n{lemma_text}"
    );
}

#[test]
fn a_wrapper_reaching_a_mutual_pair_directly_gets_a_bounded_lemma() {
    let text = export_dafny_inline(
        &program(
            "OpaqueDirect",
            "fn wrapDirect(n: Int) -> Int\n    ? \"Reaches the mutual pair through a direct call.\"\n    pingA(n)\n\n\
             verify wrapDirect law wrapDirectIsStable\n    given n: Int = [2]\n    wrapDirect(n) => 0\n",
        ),
        "aver-opaque-direct",
    );
    assert_lemma_is_bounded_and_fuelled(&text, "wrapDirect_wrapDirectIsStable");
}

#[test]
fn a_wrapper_reaching_a_mutual_pair_through_an_interpolated_string_gets_a_bounded_lemma() {
    let text = export_dafny_inline(
        &program(
            "OpaqueInterp",
            "fn wrapInterp(n: Int) -> String\n    ? \"Reaches the mutual pair only inside an interpolated string.\"\n    \"{pingA(n)}\"\n\n\
             verify wrapInterp law wrapInterpIsStable\n    given n: Int = [2]\n    wrapInterp(n) => \"0\"\n",
        ),
        "aver-opaque-interp",
    );
    assert_lemma_is_bounded_and_fuelled(&text, "wrapInterp_wrapInterpIsStable");
}

#[test]
fn a_wrapper_reaching_a_mutual_pair_through_a_map_literal_gets_a_bounded_lemma() {
    let text = export_dafny_inline(
        &program(
            "OpaqueMap",
            "fn wrapMap(n: Int) -> Map<Int, Int>\n    ? \"Reaches the mutual pair only inside a map literal.\"\n    { 0 => pingA(n) }\n\n\
             verify wrapMap law wrapMapIsStable\n    given n: Int = [2]\n    wrapMap(n) => { 0 => 0 }\n",
        ),
        "aver-opaque-maplit",
    );
    assert_lemma_is_bounded_and_fuelled(&text, "wrapMap_wrapMapIsStable");
}
