# aver-fuzz

Coverage-guided fuzz harness for the Aver lexer + parser, built on
[AFL++](https://aflplus.plus/) via the [`afl.rs`](https://rust-fuzz.github.io/book/afl.html)
crate (`cargo-afl`).

This crate sits outside the main workspace (`exclude = ["fuzz"]` in
the root `Cargo.toml`), so it does not affect regular `cargo build`
or `cargo test`.

## Why AFL++ on top of proptest

Aver's main test suite already runs `proptest` with `PROPTEST_CASES=2000`
in CI, including a random-bytes parser/lexer crash strategy
(`prop::collection::vec(any::<u8>(), 0..N)`). That covers shallow
"the lexer must not panic on arbitrary bytes" robustness.

What proptest does *not* do is coverage-guided mutation: when it
generates a byte sequence that happens to take a new code path, it
forgets and rolls fresh. AFL++ does the opposite — it preserves
"interesting" inputs (ones that hit new edges) and evolves them
further. For a language parser that gates everything behind valid
UTF-8 + valid token shapes + valid indentation, coverage feedback is
the difference between "rediscovering lexer rejection 10⁶ times" and
"reaching deep parser paths".

## Setup

One-time:

```sh
cargo install cargo-afl
cd fuzz
cargo afl build --release
```

## Running locally

Short interactive run (the AFL UI in your terminal):

```sh
cargo afl fuzz \
    -i corpus/parser \
    -o out/parser \
    -x dicts/aver.dict \
    -- target/release/fuzz_parse_bytes
```

Time-boxed for CI-like behaviour (3 minutes, no UI):

```sh
AFL_NO_UI=1 AFL_SKIP_CPUFREQ=1 AFL_NO_AFFINITY=1 \
cargo afl fuzz \
    -V 180 \
    -i corpus/parser \
    -o out/parser \
    -x dicts/aver.dict \
    -- target/release/fuzz_parse_bytes
```

## Triaging a crash

AFL++ writes crashing inputs into `out/parser/default/crashes/`. To
turn one into a committed regression:

```sh
cargo afl tmin \
    -i out/parser/default/crashes/id:000000,... \
    -o /tmp/min.av \
    -- target/release/fuzz_parse_bytes
cp /tmp/min.av ../tests/regressions/parser/$(sha256sum /tmp/min.av | cut -c1-8).av
```

`tests/parser_regressions.rs` will pick the file up by `*.av` glob and
re-run it on every `cargo test` to prevent the bug from coming back.

## What lives where

```
fuzz/
  Cargo.toml                            # outside workspace
  fuzz_targets/parse_bytes.rs           # the harness (afl::fuzz!)
  corpus/parser/                        # ~10 seed .av files
    hello.av                            # canonical
    math.av                             # arithmetic + verify
    types.av                            # sum + record
    interp.av                           # nested string interpolation
    result_option.av                    # ? operator, match
    effects.av                          # effect surface + module
    verify_law.av                       # law block
    decision.av                         # decision block
    comments.av                         # line comments
    empty.av                            # empty file
    malformed_*.av                      # known parse-error shapes
  dicts/aver.dict                       # Aver keywords + tokens
  README.md                             # this file
tests/regressions/parser/               # minimized historical crashes
tests/parser_regressions.rs             # `cargo test` gate
```

Do **not** commit the `out/` directory — it contains the live queue
and is reproducible from `corpus/` + dictionary.
