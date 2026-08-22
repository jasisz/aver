// Effectful functions of a DEPENDENCY module, exported with the same oracle
// threading the entry's own effectful functions get.
//
// Measured on the first external project: an entry whose proof cone reached a
// dependency's effectful function got `Unknown identifier `Infra.Store.get``
// out of Lean, because a dependency file emitted no effectful function at all
// — the lift pass read the entry's items only — and because a dotted call site
// (`Infra.Store.get(store, key)`) matched neither of the two bare shapes the
// call-site injection recognised, so it would have lost its
// `(path, oracle...)` prefix even had the callee been there. Two threading
// holes rode along inside a single module: a helper no `verify` block reaches
// was neither emitted nor injected (`Unknown identifier nextId`), and a tail
// call rebuilt after tail-call optimisation dropped the threaded parameters
// (`Application type mismatch ... expected BranchPath`). A cycle among lifted
// effectful functions had no `mutual` path at all — the hand-rolled ordering
// fell back to source order and left Lean a forward reference.
//
// The rule these tests hold: every module, dependency or entry, exports the
// effectful functions some proof cone reaches, lifted the same way, inside its
// own namespace; a dependency's reachability root is what any consumer's cone
// reaches; a dotted call site threads the CALLEE's own oracle list; a tail
// call threads like any other call; and a cycle is one `mutual ... end` block.
// A function nobody proves anything about is still not lifted — in a
// dependency as in the entry.
//
// The lake-backed tests are guarded by the standard `lake --version` skip.

use super::*;
use std::collections::HashMap;

fn lake_available() -> bool {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping dependency-effects proof test: `lake` not available");
        return false;
    }
    true
}

/// Write `files` into a fresh module-root dir, run `aver proof <entry>
/// --backend lean --module-root <root> -o <out>` WITHOUT `--check`, and return
/// the requested emitted files. Fast (no lake) — for assertions on text.
fn emit_multi(files: &[(&str, &str)], entry: &str, read_back: &[&str]) -> HashMap<String, String> {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-dep-effects-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    for (name, source) in files {
        let path = src.join(name);
        if let Some(dir) = path.parent() {
            std::fs::create_dir_all(dir).expect("create module dir");
        }
        std::fs::write(&path, source).unwrap_or_else(|e| panic!("write {name}: {e}"));
    }
    let out = temp_output_dir("aver-dep-effects-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(src.join(entry))
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg(&src)
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let mut leans = HashMap::new();
    for name in read_back {
        let lean = std::fs::read_to_string(out.join(name))
            .unwrap_or_else(|e| panic!("read generated {name}: {e}"));
        leans.insert((*name).to_string(), lean);
    }
    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
    leans
}

/// Asserts a lake-backed run built cleanly and every sample passed.
fn assert_builds_and_passes(summary: &serde_json::Value, run: &std::process::Output, why: &str) {
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["passed"].as_bool(),
        ),
        (Some(0), Some(true)),
        "{why}\n{}",
        format_output(run)
    );
}

// ---- The shared capability + store, the shape measured on the external
// ---- project: a keyed database declared as a capability with a resource,
// ---- and a store module whose `get` reads through it.

const KV: &str = r#"module Kv
    intent = "A keyed database, declared as a capability with a handle resource."
    kind = capability
    semantics = effectful
    exposes [Handle, open, get, count]

resource Handle

operation open(dir: String) -> Result<Handle, String>
    ? "Open the database in this directory."
    oracle = generativeOutput
    replay = recorded

operation get(key: String) -> Result<Option<String>, String>
    ? "What is stored under a key, if anything."
    oracle = generative
    replay = recorded

operation count() -> Result<Int, String>
    ? "How many keys the database holds."
    oracle = generative
    replay = recorded
"#;

const STORE: &str = r#"module Store
    intent = "A keyed store over memory or a database handle."
    exposes [Store, get, count, fixture]
    depends [Infra.Kv]
    effects [Infra.Kv.get, Infra.Kv.count]

type Store
    Memory(Map<String, String>)
    Database(Infra.Kv.Handle)

fn get(store: Store, key: String) -> Result<Option<String>, String>
    ? "What is stored under a key."
    ! [Infra.Kv.get]
    match store
        Store.Memory(held) -> Result.Ok(Map.get(held, key))
        Store.Database(handle) -> Infra.Kv.get(key)

fn count(store: Store) -> Result<Int, String>
    ? "How many keys the store holds."
    ! [Infra.Kv.count]
    match store
        Store.Memory(held) -> Result.Ok(Map.len(held))
        Store.Database(handle) -> Infra.Kv.count()

fn fixture(pairs: List<Tuple<String, String>>) -> Store
    ? "A store holding exactly these pairs."
    Store.Memory(Map.fromList(pairs))
"#;

// ---------------------------------------------------------------------------
// (a) The entry's proof cone reaches a dependency's effectful function.
// ---------------------------------------------------------------------------

const AUDIT_ENTRY: &str = r#"module Audit
    intent = "Reads the store through an effectful wrapper and proves what it answers."
    exposes [main, named, heightKey]
    depends [Infra.Kv, Infra.Store]
    effects [Infra.Kv.get]

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"

verify heightKey
    heightKey(5) => "h:5"

fn named(store: Store, height: Int) -> Result<Option<String>, String>
    ? "The block id the store names at a height."
    ! [Infra.Kv.get]
    Infra.Store.get(store, heightKey(height))

verify named
    given stub: Infra.Kv.get = [fixedGet]
    named(Infra.Store.fixture([("h:5", "aa")]), 5) => Result.Ok(Option.Some("aa"))
    named(Infra.Store.fixture([]), 5) => Result.Ok(Option.None)

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

#[test]
fn dependency_effectful_fn_is_exported_with_the_entrys_oracle_threading() {
    // Red before this change:
    //   error: Audit.lean:188:2: Unknown identifier `Infra.Store.get`
    // plus the two sampled cases cascading through
    //   Tactic `native_decide` failed: ... 'Audit.named' uses 'sorry'
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", STORE),
            ("audit.av", AUDIT_ENTRY),
        ],
        "audit.av",
        &["Infra/Store.lean", "Audit.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "an entry whose cone reaches a dependency's effectful function must build",
    );
    let store = &leans["Infra/Store.lean"];
    assert!(
        store.contains(
            "def get (path : BranchPath) (rnd_Infra_Kv_get : BranchPath \u{2192} Int \u{2192} String \u{2192} Except String (Option String)) (store : Store) (key : String)"
        ),
        "the dependency's effectful `get` must be lifted with the same \
         `(path : BranchPath)` + oracle parameters the entry's own get:\n{store}"
    );
    // The filter's purpose survives the move: `count` is effectful too, and
    // no cone reaches it, so it is not lifted.
    assert!(
        !store.contains("def count"),
        "an effectful dependency function no proof cone reaches must NOT be \
         lifted:\n{store}"
    );
    let entry = &leans["Audit.lean"];
    assert!(
        entry.contains("Infra.Store.get path rnd_Infra_Kv_get store (heightKey height)"),
        "the entry's dotted call site must pass the callee's own `(path, \
         oracle...)` prefix:\n{entry}"
    );
}

// ---------------------------------------------------------------------------
// (b) A dependency helper no `verify` block reaches, but the entry's cone
// ---------------------------------------------------------------------------

const CHAIN: &str = r#"module Chain
    intent = "Walks the index held in a store."
    exposes [parentOf]
    depends [Infra.Store]
    effects [Infra.Kv.get]

fn parentOf(store: Store, height: Int) -> Result<Option<String>, String>
    ? "The block id named one height below, if there is one."
    ! [Infra.Kv.get]
    match height <= 0
        true -> Result.Ok(Option.None)
        false -> named(store, height - 1)

fn named(store: Store, height: Int) -> Result<Option<String>, String>
    ? "The block id the store names at a height."
    ! [Infra.Kv.get]
    Infra.Store.get(store, heightKey(height))

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"
"#;

const CHAIN_ENTRY: &str = r#"module Main
    intent = "Proves a claim whose cone reaches a dependency helper nobody verifies."
    exposes [main, parentAt]
    depends [Infra.Kv, Infra.Chain, Infra.Store]
    effects [Infra.Kv.get]

fn parentAt(store: Store, height: Int) -> Result<Option<String>, String>
    ? "The parent the chain names at a height."
    ! [Infra.Kv.get]
    Infra.Chain.parentOf(store, height)

verify parentAt
    given stub: Infra.Kv.get = [fixedGet]
    parentAt(Infra.Store.fixture([("h:4", "aa")]), 5) => Result.Ok(Option.Some("aa"))
    parentAt(Infra.Store.fixture([("h:4", "aa")]), 0) => Result.Ok(Option.None)

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

#[test]
fn dependency_helper_reached_only_through_the_entry_is_exported() {
    // Red before this change:
    //   error: Main.lean:186:2: Unknown identifier `Infra.Chain.parentOf`
    // `Infra.Chain.parentOf` is reached from the entry's `verify parentAt`,
    // and it in turn calls `named`, which no `verify` block anywhere names.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", STORE),
            ("infra/chain.av", CHAIN),
            ("main.av", CHAIN_ENTRY),
        ],
        "main.av",
        &["Infra/Chain.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a dependency helper reachable only through the entry's cone must be exported",
    );
    let chain = &leans["Infra/Chain.lean"];
    for name in ["def parentOf", "def named"] {
        assert!(
            chain.contains(name),
            "`{name}` must be exported: the reachability root of a dependency \
             is what the consumer's cone reaches, closed over the module's own \
             calls:\n{chain}"
        );
    }
}

// ---------------------------------------------------------------------------
// (c) A tail call after tail-call optimisation threads like any other call.
// ---------------------------------------------------------------------------

const WALK: &str = r#"module Walk
    intent = "Counts the heights an index names, looping until a gap."
    exposes [heightsHeld]
    depends [Infra.Store]
    effects [Infra.Kv.get]

fn heightsHeld(store: Store, height: Int) -> Result<Int, String>
    ? "How many heights the index names, counting up from a height."
    ! [Infra.Kv.get]
    named = Infra.Store.get(store, heightKey(height))?
    match named
        Option.None -> Result.Ok(height)
        Option.Some(blockId) -> heightsHeld(store, height + 1)

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"
"#;

const WALK_ENTRY: &str = r#"module Main
    intent = "Proves a claim about a dependency loop over a store effect."
    exposes [main, held]
    depends [Infra.Kv, Infra.Store, Infra.Walk]
    effects [Infra.Kv.get]

fn held(store: Store) -> Result<Int, String>
    ? "How many heights the index names from genesis."
    ! [Infra.Kv.get]
    Infra.Walk.heightsHeld(store, 0)

verify held
    given stub: Infra.Kv.get = [fixedGet]
    held(Infra.Store.fixture([])) => Result.Ok(0)
    held(Infra.Store.fixture([("h:0", "aa"), ("h:1", "bb")])) => Result.Ok(2)

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

const PRUNE_ENTRY_LOOP: &str = r#"module Prune
    intent = "Counts the heights an index names, looping until a gap."
    exposes [main, heightsHeld]
    depends [Infra.Kv, Infra.Store]
    effects [Infra.Kv.get]

fn heightsHeld(store: Store, height: Int) -> Result<Int, String>
    ? "How many heights the index names, counting up from a height."
    ! [Infra.Kv.get]
    named = Infra.Store.get(store, heightKey(height))?
    match named
        Option.None -> Result.Ok(height)
        Option.Some(blockId) -> heightsHeld(store, height + 1)

verify heightsHeld
    given stub: Infra.Kv.get = [fixedGet]
    heightsHeld(Infra.Store.fixture([]), 0) => Result.Ok(0)
    heightsHeld(Infra.Store.fixture([("h:0", "aa"), ("h:1", "bb")]), 0) => Result.Ok(2)

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

#[test]
fn dependency_effectful_loop_forwards_path_and_oracles_through_its_tail_call() {
    // Red before this change:
    //   error: Main.lean:186:2: Unknown identifier `Infra.Walk.heightsHeld`
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", STORE),
            ("infra/walk.av", WALK),
            ("main.av", WALK_ENTRY),
        ],
        "main.av",
        &["Infra/Walk.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a dependency's self-recursive effectful loop must build",
    );
    let walk = &leans["Infra/Walk.lean"];
    assert!(
        walk.contains("heightsHeld path rnd_Infra_Kv_get store (height + 1)"),
        "the tail call must forward `path` and the oracles — without them the \
         first source argument lands on the `BranchPath` parameter:\n{walk}"
    );
}

#[test]
fn entry_effectful_loop_over_a_dependency_read_forwards_path_and_oracles() {
    // The same loop written in the entry: red before this change on the
    // dotted read it makes,
    //   error: Prune.lean:188:8: Unknown identifier `Infra.Store.get`
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", STORE),
            ("prune.av", PRUNE_ENTRY_LOOP),
        ],
        "prune.av",
        &["Prune.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "an entry's self-recursive effectful loop over a dependency read must build",
    );
    let prune = &leans["Prune.lean"];
    assert!(
        prune.contains("heightsHeld path rnd_Infra_Kv_get store (height + 1)"),
        "the entry's tail call must forward `path` and the oracles:\n{prune}"
    );
}

// ---------------------------------------------------------------------------
// (d) Two effectful functions calling each other: ONE `mutual ... end` block.
// ---------------------------------------------------------------------------

const FLOOR: &str = r#"module Floor
    intent = "Finds the lowest segment a surviving height sits in, two functions calling each other."
    exposes [lowestAtOrAbove]
    depends [Infra.Store]
    effects [Infra.Kv.get]

fn lowestAtOrAbove(store: Store, height: Int, best: Int) -> Result<Int, String>
    ? "Walk the heights the index names, keeping the lowest segment used."
    ! [Infra.Kv.get]
    named = Infra.Store.get(store, heightKey(height))?
    match named
        Option.None -> Result.Ok(best)
        Option.Some(blockId) -> lowestWith(store, height, blockId, best)

fn lowestWith(store: Store, height: Int, blockId: String, best: Int) -> Result<Int, String>
    ? "A height the index names: lower the answer only if its block is held."
    ! [Infra.Kv.get]
    located = Infra.Store.get(store, blockKey(blockId))?
    match located
        Option.None -> lowestAtOrAbove(store, height + 1, best)
        Option.Some(text) -> lowestAtOrAbove(store, height + 1, lower(best, String.len(text)))

fn lower(a: Int, b: Int) -> Int
    ? "The smaller of two segment numbers."
    match a <= b
        true -> a
        false -> b

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"

fn blockKey(blockId: String) -> String
    ? "The key naming a block."
    "b:{blockId}"
"#;

const FLOOR_ENTRY: &str = r#"module Main
    intent = "Proves a claim about a dependency cycle over a store effect."
    exposes [main, floorFrom]
    depends [Infra.Kv, Infra.Floor, Infra.Store]
    effects [Infra.Kv.get]

fn floorFrom(store: Store, below: Int) -> Result<Int, String>
    ? "The lowest segment that has to survive."
    ! [Infra.Kv.get]
    Infra.Floor.lowestAtOrAbove(store, below, 9)

verify floorFrom
    given stub: Infra.Kv.get = [fixedGet]
    floorFrom(Infra.Store.fixture([]), 5) => Result.Ok(9)
    floorFrom(Infra.Store.fixture([("h:5", "aa"), ("b:aa", "3:0")]), 5) => Result.Ok(3)

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

const PRUNE_ENTRY_CYCLE: &str = r#"module Prune
    intent = "Finds the lowest segment a surviving height sits in, two functions calling each other."
    exposes [main, lowestAtOrAbove]
    depends [Infra.Kv, Infra.Store]
    effects [Infra.Kv.get]

fn lowestAtOrAbove(store: Store, height: Int, best: Int) -> Result<Int, String>
    ? "Walk the heights the index names, keeping the lowest segment used."
    ! [Infra.Kv.get]
    named = Infra.Store.get(store, heightKey(height))?
    match named
        Option.None -> Result.Ok(best)
        Option.Some(blockId) -> lowestWith(store, height, blockId, best)

verify lowestAtOrAbove
    given stub: Infra.Kv.get = [fixedGet]
    lowestAtOrAbove(Infra.Store.fixture([]), 5, 9) => Result.Ok(9)
    lowestAtOrAbove(Infra.Store.fixture([("h:5", "aa"), ("b:aa", "3:0")]), 5, 9) => Result.Ok(3)

fn lowestWith(store: Store, height: Int, blockId: String, best: Int) -> Result<Int, String>
    ? "A height the index names: lower the answer only if its block is held."
    ! [Infra.Kv.get]
    located = Infra.Store.get(store, blockKey(blockId))?
    match located
        Option.None -> lowestAtOrAbove(store, height + 1, best)
        Option.Some(text) -> lowestAtOrAbove(store, height + 1, lower(best, String.len(text)))

fn lower(a: Int, b: Int) -> Int
    ? "The smaller of two segment numbers."
    match a <= b
        true -> a
        false -> b

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"

fn blockKey(blockId: String) -> String
    ? "The key naming a block."
    "b:{blockId}"

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

#[test]
fn mutually_recursive_dependency_effectful_fns_are_emitted_in_one_mutual_block() {
    // Red before this change:
    //   error: Main.lean:186:2: Unknown identifier `Infra.Floor.lowestAtOrAbove`
    // and, once the pair was emitted at all, the previous hand-rolled ordering
    // had no cycle branch: it fell back to source order and left Lean a
    // forward reference to the second member.
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", STORE),
            ("infra/floor.av", FLOOR),
            ("main.av", FLOOR_ENTRY),
        ],
        "main.av",
        &["Infra/Floor.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a cycle among a dependency's effectful functions must build, never go red",
    );
    let floor = &leans["Infra/Floor.lean"];
    let mutual_block = floor
        .split_once("mutual")
        .map(|(_, rest)| rest.split_once("\nend\n").map(|(b, _)| b).unwrap_or(rest))
        .unwrap_or_else(|| panic!("a cycle must be emitted in one `mutual` block:\n{floor}"));
    for name in ["def lowestAtOrAbove", "def lowestWith"] {
        assert!(
            mutual_block.contains(name),
            "`{name}` must be INSIDE the `mutual` block — a member emitted \
             outside it is the forward reference Lean rejects:\n{floor}"
        );
    }
}

#[test]
fn mutually_recursive_entry_effectful_fns_over_a_dependency_read_build() {
    // The same cycle written in the entry: red before this change with
    //   error: Prune.lean:197:8: Unknown identifier `Infra.Store.get`
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", STORE),
            ("prune.av", PRUNE_ENTRY_CYCLE),
        ],
        "prune.av",
        &["Prune.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a cycle among the entry's effectful functions must build",
    );
    let prune = &leans["Prune.lean"];
    assert!(
        prune.contains("mutual"),
        "the entry's effectful cycle must be one `mutual` block:\n{prune}"
    );
}

// ---------------------------------------------------------------------------
// (e) The measured shape end to end: capability, opaque store, audit entry
// ---------------------------------------------------------------------------

const OPAQUE_STORE: &str = r#"module Store
    intent = "A keyed store over memory or a database handle, opaque to its callers."
    exposes [get, count, fixture]
    exposes opaque [Store]
    depends [Infra.Kv]
    effects [Infra.Kv.get, Infra.Kv.count]

type Store
    Memory(Map<String, String>)
    Database(Infra.Kv.Handle)

fn get(store: Store, key: String) -> Result<Option<String>, String>
    ? "What is stored under a key."
    ! [Infra.Kv.get]
    match store
        Store.Memory(held) -> Result.Ok(Map.get(held, key))
        Store.Database(handle) -> Infra.Kv.get(key)

verify get
    get(fixture([("a", "b")]), "a") => Result.Ok(Option.Some("b"))
    get(fixture([]), "a") => Result.Ok(Option.None)

fn count(store: Store) -> Result<Int, String>
    ? "How many keys the store holds."
    ! [Infra.Kv.count]
    match store
        Store.Memory(held) -> Result.Ok(Map.len(held))
        Store.Database(handle) -> Infra.Kv.count()

fn fixture(pairs: List<Tuple<String, String>>) -> Store
    ? "A store holding exactly these pairs, for verify blocks in other modules."
    Store.Memory(Map.fromList(pairs))

verify fixture
    fixture([]) => Store.Memory(Map.fromList([]))
"#;

const AUDIT_MODULE: &str = r#"module Audit
    intent = "Walks the index in a store and reports what it finds."
    exposes [Counts, standing, parentOf, named, nonEmpty, sizeOf, said]
    depends [Infra.Kv, Infra.Store]
    effects [Console.print, Infra.Kv.count, Infra.Kv.get]

record Counts
    checked: Int
    faults: Int

fn standing(counts: Counts) -> String
    ? "One line saying how the audit stands."
    match counts.faults == 0
        true -> "{counts.checked} checked, clean"
        false -> "{counts.checked} checked, {counts.faults} faults"

verify standing
    standing(Counts(checked = 3, faults = 0)) => "3 checked, clean"
    standing(Counts(checked = 3, faults = 1)) => "3 checked, 1 faults"

fn parentOf(store: Store, height: Int) -> Result<Option<String>, String>
    ? "The block id named one height below, if there is one."
    ! [Infra.Kv.get]
    match height <= 0
        true -> Result.Ok(Option.None)
        false -> named(store, height - 1)

verify parentOf
    given stub: Infra.Kv.get = [fixedGet]
    parentOf(Infra.Store.fixture([("h:4", "aa")]), 5) => Result.Ok(Option.Some("aa"))
    parentOf(Infra.Store.fixture([]), 0) => Result.Ok(Option.None)

fn named(store: Store, height: Int) -> Result<Option<String>, String>
    ? "The block id the store names at a height, refusing an empty one."
    ! [Infra.Kv.get]
    found = Infra.Store.get(store, heightKey(height))?
    match found
        Option.None -> Result.Ok(Option.None)
        Option.Some(blockId) -> nonEmpty(height, blockId)

verify named
    given stub: Infra.Kv.get = [fixedGet]
    named(Infra.Store.fixture([("h:5", "aa")]), 5) => Result.Ok(Option.Some("aa"))
    named(Infra.Store.fixture([("h:5", "")]), 5) => Result.Err("index has no block id at height 5")
    named(Infra.Store.fixture([]), 5) => Result.Ok(Option.None)

fn nonEmpty(height: Int, blockId: String) -> Result<Option<String>, String>
    ? "A height the index names has to name something."
    match String.len(blockId) > 0
        true -> Result.Ok(Option.Some(blockId))
        false -> Result.Err("index has no block id at height {height}")

verify nonEmpty
    nonEmpty(5, "aa") => Result.Ok(Option.Some("aa"))
    nonEmpty(5, "") => Result.Err("index has no block id at height 5")

fn sizeOf(store: Store) -> Result<Int, String>
    ? "How many entries the index holds."
    ! [Infra.Kv.count]
    held = Infra.Store.count(store)?
    Result.Ok(held)

verify sizeOf
    given stub: Infra.Kv.count = [fixedCount]
    sizeOf(Infra.Store.fixture([("h:0", "aa")])) => Result.Ok(1)
    sizeOf(Infra.Store.fixture([])) => Result.Ok(0)

fn said(counts: Counts, lines: List<String>) -> Counts
    ? "Print the lot in one write and hand the counts back."
    ! [Console.print]
    _printed = Console.print(String.join(lines, "\n"))
    counts

verify said trace
    said(Counts(checked = 1, faults = 0), []) => Counts(checked = 1, faults = 0)

fn heightKey(height: Int) -> String
    ? "The key naming a height."
    "h:{height}"

verify heightKey
    heightKey(5) => "h:5"

fn fixedGet(path: BranchPath, n: Int, key: String) -> Result<Option<String>, String>
    ? "A concrete proof-side stub for the database read."
    Result.Ok(Option.None)

fn fixedCount(path: BranchPath, n: Int) -> Result<Int, String>
    ? "A concrete proof-side stub for the database count."
    Result.Ok(0)
"#;

#[test]
fn capability_store_and_audit_entry_build_end_to_end() {
    // The shape the external project's `infra/audit.av` has, cut to its
    // smallest form: a capability with a resource, a store module that hides
    // its type and reads through the capability, and an audit module with
    // several `verify` blocks whose cones cross into it. Red before this
    // change with eleven build errors, the first
    //   error: Audit.lean:208:8: Unknown identifier `Infra.Store.get`
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[
            ("infra/kv.av", KV),
            ("infra/store.av", OPAQUE_STORE),
            ("infra/audit.av", AUDIT_MODULE),
        ],
        "infra/audit.av",
        &["Infra/Store.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "the measured audit shape must build end to end",
    );
    let store = &leans["Infra/Store.lean"];
    for name in [
        "def get (path : BranchPath)",
        "def count (path : BranchPath)",
    ] {
        assert!(
            store.contains(name),
            "`{name}`: both reads the audit's cones reach must be lifted:\n{store}"
        );
    }
}

// ---------------------------------------------------------------------------
// (f) Nothing moves for a program without a reached effectful dependency fn.
// ---------------------------------------------------------------------------

/// A dependency with a pure function the entry proves something about, and an
/// effectful one nobody's cone reaches.
const QUIET_WITH_UNREACHED_EFFECT: &str = r#"module Quiet
    intent = "Reports a total, and separately loops over the console forever."
    exposes [total, chatter]
    effects [Console.print]

fn total(items: List<Int>) -> Int
    ? "The sum of the items."
    match items
        [] -> 0
        [head, ..tail] -> head + total(tail)

fn chatter(n: Int) -> Unit
    ? "Say the number, then say the next one, and never stop."
    ! [Console.print]
    _said = Console.print("{n}")
    chatter(n + 1)
"#;

/// The same dependency with the effectful function deleted.
const QUIET_WITHOUT_EFFECT: &str = r#"module Quiet
    intent = "Reports a total."
    exposes [total]
    effects []

fn total(items: List<Int>) -> Int
    ? "The sum of the items."
    match items
        [] -> 0
        [head, ..tail] -> head + total(tail)
"#;

const QUIET_ENTRY: &str = r#"module Demo
    intent = "Proves what the dependency's total answers."
    depends [Quiet]
    effects []

fn sum(items: List<Int>) -> Int
    ? "The sum, through the dependency."
    Quiet.total(items)

verify sum
    sum([]) => 0
    sum([1, 2]) => 3
"#;

#[test]
fn a_dependency_effectful_fn_no_cone_reaches_leaves_emission_byte_identical() {
    // The entry filter's rationale, kept module-aware: an effectful loop
    // nobody proves anything about (`chatter` prints forever) is not lifted in
    // a dependency either — so the dependency file is byte-for-byte what the
    // same program emits with that function deleted from the source. Fast (no
    // lake).
    let with_effect = emit_multi(
        &[
            ("quiet.av", QUIET_WITH_UNREACHED_EFFECT),
            ("demo.av", QUIET_ENTRY),
        ],
        "demo.av",
        &["Quiet.lean"],
    );
    let without_effect = emit_multi(
        &[("quiet.av", QUIET_WITHOUT_EFFECT), ("demo.av", QUIET_ENTRY)],
        "demo.av",
        &["Quiet.lean"],
    );
    assert_eq!(
        with_effect["Quiet.lean"], without_effect["Quiet.lean"],
        "an effectful dependency function no proof cone reaches must leave the \
         emitted module byte-identical to the same program without it"
    );
    assert!(
        !with_effect["Quiet.lean"].contains("BranchPath"),
        "no oracle threading belongs in a module that lifts nothing:\n{}",
        with_effect["Quiet.lean"]
    );
}

#[test]
fn a_cross_module_program_without_effects_emits_the_golden_bytes() {
    // The corpus no-movement guarantee, on a checked-in golden: a two-module
    // program with no effects at all emits exactly the bytes
    // `tests/fixtures/map_order_cross_module.baseline.lean` records, and its
    // dependency file carries no oracle threading. Fast (no lake).
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = temp_output_dir("aver-dep-effects-golden");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/map_order_cross_module/main.av")
        .arg("--backend")
        .arg("lean")
        .arg("--module-root")
        .arg("tests/fixtures/map_order_cross_module")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let emitted =
        std::fs::read_to_string(out.join("MapOrderCrossModule.lean")).expect("read emitted entry");
    let dependency = std::fs::read_to_string(out.join("MapKeys.lean")).expect("read emitted dep");
    let _ = std::fs::remove_dir_all(&out);
    let baseline = std::fs::read_to_string(
        repo_root.join("tests/fixtures/map_order_cross_module.baseline.lean"),
    )
    .expect("read baseline golden Lean");
    assert_eq!(
        emitted, baseline,
        "a program with no effectful dependency function must emit the golden bytes"
    );
    assert!(
        !dependency.contains("BranchPath") && !dependency.contains("rnd_"),
        "the dependency file must carry no oracle threading:\n{dependency}"
    );
}

// ---------------------------------------------------------------------------
// (f) The types an effect carries belong to modules the source never writes
// ---------------------------------------------------------------------------
//
// A lifted signature spells the types of its effects by their owner module —
// `Tcp.Connection`, `Bytes.Bytes` — and that owner is a standard module the
// loader pulled in behind a call, not a written `depends`. While only the
// entry lifted effectful functions this cost nothing: the entry imports every
// module of the project. A dependency file that lifts one has to import those
// owners itself.

const SESSION_DEP: &str = r#"module Sess
    intent = "One line over one connection."
    exposes [greet]
    effects [Tcp.connect, Tcp.writeLine, Tcp.close]

fn greet(host: String, port: Int, line: String) -> Result<Int, String>
    ? "Open a session, write one line, close it."
    ! [Tcp.connect, Tcp.writeLine, Tcp.close]
    conn = Tcp.connect(host, port)?
    Tcp.writeLine(conn, line)?
    Tcp.close(conn)?
    Result.Ok(String.len(line))
"#;

const SESSION_ENTRY: &str = r#"module Wire
    intent = "Proves what the session writes when it is skipped."
    exposes [main, sent]
    depends [Infra.Sess]
    effects [Tcp.connect, Tcp.writeLine, Tcp.close]

fn sent(line: String, skip: Bool) -> Result<Int, String>
    ? "How many characters the session wrote."
    ! [Tcp.connect, Tcp.writeLine, Tcp.close]
    match skip
        true -> Result.Ok(0)
        false -> Infra.Sess.greet("localhost", 9, line)

verify sent
    sent("abc", true) => Result.Ok(0)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

/// The `import` lines a generated Lean file opens with.
fn import_lines(lean: &str) -> Vec<&str> {
    lean.lines()
        .take_while(|line| line.starts_with("import"))
        .collect()
}

#[test]
fn a_dependency_lifting_a_connection_effect_imports_the_module_owning_it() {
    // Red before this change — `Infra/Sess.lean` opened with `import
    // AverCommon` and nothing else, so Lean lost the type once per occurrence
    // in the lifted signature:
    //   error: Infra/Sess.lean:10:68: Unknown identifier `Tcp.Connection`
    //   error: Infra/Sess.lean:10:114: Unknown identifier `Tcp.Connection`
    //   error: Infra/Sess.lean:10:170: Unknown identifier `Tcp.Connection`
    //   error: Infra/Sess.lean:10:252: Unknown identifier `Tcp.Connection`
    //   error: Infra/Sess.lean:10:313: Unknown identifier `Tcp.Connection`
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[("infra/sess.av", SESSION_DEP), ("wire.av", SESSION_ENTRY)],
        "wire.av",
        &["Infra/Sess.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a dependency whose lifted signature names `Tcp.Connection` must build",
    );
    let sess = &leans["Infra/Sess.lean"];
    assert!(
        sess.contains("Tcp.Connection"),
        "the lifted oracle parameters must spell the connection type:\n{sess}"
    );
    assert!(
        import_lines(sess).contains(&"import Tcp"),
        "the file names `Tcp.Connection`, so it must import `Tcp`:\n{sess}"
    );
}

const BLOB_DEP: &str = r#"module Blob
    intent = "Reads a file as bytes."
    exposes [sizeOf]
    effects [Disk.readBytes]

fn sizeOf(file: String) -> Result<Int, String>
    ? "Whether the file could be read at all."
    ! [Disk.readBytes]
    match Disk.readBytes(file)
        Result.Ok(payload) -> Result.Ok(1)
        Result.Err(reason) -> Result.Err(reason)
"#;

const BLOB_ENTRY: &str = r#"module Sizes
    intent = "Proves what the read answers when it is skipped."
    exposes [main, measured]
    depends [Infra.Blob]
    effects [Disk.readBytes]

fn measured(file: String, skip: Bool) -> Result<Int, String>
    ? "One when the file could be read, zero when the read is skipped."
    ! [Disk.readBytes]
    match skip
        true -> Result.Ok(0)
        false -> Infra.Blob.sizeOf(file)

verify measured
    measured("missing.bin", true) => Result.Ok(0)

fn main() -> Unit
    ? "Entry point."
    Unit
"#;

#[test]
fn a_dependency_lifting_a_bytes_effect_imports_the_module_owning_bytes() {
    // Red before this change:
    //   error: Infra/Blob.lean:10:95: Unknown identifier `Bytes.Bytes`
    if !lake_available() {
        return;
    }
    let (summary, run, leans) = super::cross_file::run_multi(
        &[("infra/blob.av", BLOB_DEP), ("sizes.av", BLOB_ENTRY)],
        "sizes.av",
        &["Infra/Blob.lean"],
    );
    assert_builds_and_passes(
        &summary,
        &run,
        "a dependency whose lifted signature names `Bytes.Bytes` must build",
    );
    let blob = &leans["Infra/Blob.lean"];
    assert!(
        blob.contains("Bytes.Bytes"),
        "the lifted oracle parameter must spell the bytes type:\n{blob}"
    );
    assert!(
        import_lines(blob).contains(&"import Bytes"),
        "the file names `Bytes.Bytes`, so it must import `Bytes`:\n{blob}"
    );
}

/// A dependency that reaches `Tcp` from a function no cone reaches — so it
/// lifts nothing — and whose intent names the connection type in prose.
const HUSH_DEP: &str = r#"module Hush
    intent = "Adds numbers, and can also poke a port."
    exposes [total]
    effects [Tcp.connect, Tcp.close]

fn total(items: List<Int>) -> Int
    ? "The sum of the items. Nothing here holds a Tcp.Connection."
    match items
        [] -> 0
        [head, ..tail] -> head + total(tail)

fn poke(host: String, port: Int) -> Result<Unit, String>
    ? "Open a connection and drop it again."
    ! [Tcp.connect, Tcp.close]
    conn = Tcp.connect(host, port)?
    Tcp.close(conn)
"#;

const HUSH_ENTRY: &str = r#"module Adder
    intent = "Proves what the dependency's total answers."
    depends [Hush]
    effects []

fn sum(items: List<Int>) -> Int
    ? "The sum, through the dependency."
    Hush.total(items)

verify sum
    sum([]) => 0
    sum([1, 2]) => 3
"#;

#[test]
fn a_dependency_that_lifts_nothing_keeps_the_imports_it_had() {
    // The narrowing the widening above needs. `Tcp` is a module of this
    // project — `poke` calls it, so the loader pulls it in — but no cone
    // reaches `poke`, so `Hush.lean` lifts nothing and names no `Tcp`
    // constant. Its imports stay what they were: `import AverCommon`, alone.
    // `total`'s intent mentions `Tcp.Connection` in prose to hold the same
    // line from the other side — a module path in a doc comment is not a
    // reference. Fast (no lake).
    let leans = emit_multi(
        &[("hush.av", HUSH_DEP), ("adder.av", HUSH_ENTRY)],
        "adder.av",
        &["Hush.lean", "Adder.lean"],
    );
    let hush = &leans["Hush.lean"];
    assert_eq!(
        import_lines(hush),
        vec!["import AverCommon"],
        "a module that lifts nothing must keep the imports it had:\n{hush}"
    );
    assert!(
        hush.contains("Tcp.Connection"),
        "the doc comment carrying the intent must still be there, or this \
         test proves nothing:\n{hush}"
    );
    assert!(
        !hush.contains("BranchPath"),
        "no oracle threading belongs in a module that lifts nothing:\n{hush}"
    );
    // The control from the other side: the entry imports every module of the
    // project, `Tcp` included, exactly as it did before.
    assert!(
        import_lines(&leans["Adder.lean"]).contains(&"import Tcp"),
        "the entry imports the whole project:\n{}",
        leans["Adder.lean"]
    );
}
