// Coverage-guided fuzz target: arbitrary bytes → replay JSON codec.
//
// Pre-Iron the replay codec was exercised only by `tests/replay_proptest.rs`,
// which generates *structured* `Value` shapes and round-trips them through
// `value_to_json → json_to_value`. That found the two B3 bugs (Vector
// decode missing, single-key Map marker collision) but its generator is
// bounded by what `arb_replay_safe_value` knows how to produce — it
// can never generate an *invalid* JSON shape the codec hasn't anticipated.
//
// This target attacks the codec from the other side: random bytes
// claiming to be replay JSON. The pipeline:
//
//   bytes → utf8 → parse_json → json_to_value → value_to_json
//
// Each stage may legitimately reject the input with an `Err`. The
// invariant under test is the same as the other two targets: no panic,
// no abort, no stack overflow. Replay files come from real recordings
// the user might hand-edit or transport across systems, so a panicking
// decoder is the same class of bug as a panicking parser — one bad
// input takes down `aver replay <file>` for the whole session.
//
// Round-trips that survive `json_to_value` get a second pass through
// `value_to_json` to exercise the encoder's own panic surface (the
// non-finite-float and unsupported-variant guards live there).
//
// Iron 0.21 Hardcore Fuzz Phase 0: metrics surface to
// `/tmp/aver_fuzz_metrics_fuzz_replay_codec.txt`. The `lex_ok` counter
// re-purposes for "parse_json succeeded" — same semantic role
// (input passes the first parser layer). `parse_ok` is "json_to_value
// succeeded". `ast_node_sum` / `max_ast_depth` stay zero here
// because we don't construct a `Vec<TopLevel>` for replay JSON.

#[path = "common.rs"]
mod common;

fn main() {
    afl::fuzz!(|data: &[u8]| {
        let c = common::counters();
        c.record_exec();
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };
        let Ok(json) = aver::replay::json::parse_json(source) else {
            return;
        };
        c.record_lex_ok();
        let Ok(value) = aver::replay::json::json_to_value(&json) else {
            return;
        };
        c.record_parse_ok(0, 0);
        // Re-encode to exercise the encoder's variant arms with a
        // value tree that *survived* a real decode. Tightens the
        // round-trip invariant the B3 proptest already locks for the
        // generator side.
        let _ = aver::replay::json::value_to_json(&value);
    });
}
