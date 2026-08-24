use super::*;

#[test]
fn proof_export_escapes_lean_reserved_identifiers_end_to_end() {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture = repo_root.join("tests/fixtures/lean_reserved_identifiers.av");
    let root = temp_output_dir("aver-proof-lean-reserved-identifiers");

    let mut command = Command::new(aver_bin);
    command
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&fixture)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&root);
    if Command::new("lake").arg("--version").output().is_ok() {
        command.arg("--check");
    }
    let output = command
        .output()
        .expect("aver proof --backend lean for reserved identifiers");
    assert!(
        output.status.success(),
        "Lean rejected escaped reserved identifiers:\n{}",
        format_output(&output)
    );

    let lean = std::fs::read_to_string(root.join("LeanReservedIdentifiers.lean"))
        .expect("read LeanReservedIdentifiers.lean");
    for name in [
        "at",
        "using",
        "exists",
        "sorry",
        "suffices",
        "variable",
        "universe",
        "notation",
        "attribute",
    ] {
        assert!(
            lean.contains(&format!("def {name}'")),
            "reserved identifier {name} was not escaped:\n{lean}"
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_cross_module_recursive_fns_get_per_module_fn_contracts() {
    // Round-5 audit follow-up: two dep modules each declaring a
    // recursive `countdown(n: Int) -> Int` with the canonical
    // IntCountdown shape used to emit `partial def` in both modules
    // even though the standalone single-module export produced a
    // proper fuel-encoded def. Two coupled gaps:
    //   1. The proof-lower pipeline built `inputs.recursive_fns`
    //      from entry's analyze only — module fns never reached the
    //      IntCountdown classifier (entry has no countdown → empty
    //      entry-recursive set).
    //   2. `populate_fn_contracts` keyed `ir.fn_contracts` by bare
    //      fn name, so even when both modules' contracts were
    //      populated they collided on `"countdown"`.
    // Round-5 plumbs union'd `recursive_fns` through pipeline AND
    // keys `fn_contracts` by canonical `Module.fn`. Lookup-side
    // helpers `find_fn_contract` / `fn_contract_exists` walk back
    // to the canonical slot.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-cross-module-fn-contracts");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("CountdownA.av"),
        "module CountdownA\n\
         \x20   exposes [countdown]\n\
         \x20   intent = \"Plain countdown.\"\n\
         \x20   effects []\n\
         \n\
         fn countdown(n: Int) -> Int\n\
         \x20   ? \"Countdown to 0.\"\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> countdown(n - 1)\n",
    )
    .expect("write CountdownA.av");
    std::fs::write(
        root.join("CountdownB.av"),
        "module CountdownB\n\
         \x20   exposes [countdown]\n\
         \x20   intent = \"Sum on countdown.\"\n\
         \x20   effects []\n\
         \n\
         fn countdown(n: Int) -> Int\n\
         \x20   ? \"Countdown summing n.\"\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> n + countdown(n - 1)\n",
    )
    .expect("write CountdownB.av");
    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [CountdownA, CountdownB]\n\
         \x20   intent = \"Touch both modules so each surfaces in proof IR.\"\n\
         \n\
         fn main() -> Int\n\
         \x20   0\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let a_lean =
        std::fs::read_to_string(out_dir.join("CountdownA.lean")).expect("read CountdownA.lean");
    let b_lean =
        std::fs::read_to_string(out_dir.join("CountdownB.lean")).expect("read CountdownB.lean");

    assert!(
        a_lean.contains("def countdown__fuel"),
        "CountdownA.countdown must emit fuel-encoded def, not `partial def`:\n{a_lean}"
    );
    assert!(
        b_lean.contains("def countdown__fuel"),
        "CountdownB.countdown must emit fuel-encoded def, not `partial def`:\n{b_lean}"
    );
    assert!(
        !a_lean.contains("partial def countdown"),
        "CountdownA.lean must not regress to `partial def countdown`:\n{a_lean}"
    );
    assert!(
        !b_lean.contains("partial def countdown"),
        "CountdownB.lean must not regress to `partial def countdown`:\n{b_lean}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_module_owned_native_guarded_resolves_correct_fn_id() {
    // PR 12 Scope A finalization: the Lean native-guarded emit path
    // (`emit_native_guarded_int_countdown_fn`) used to derive the
    // recursive fn's `FnId` via `FnKey::entry(&fd.name)`. For any
    // module-owned native-guarded recursive fn that would either
    // panic on the missing entry slot, or silently target an
    // entry-scope same-bare-name fn. After the followup commit the
    // lookup goes through `fn_id_for_decl(ctx, fd)` — pointer-eq
    // scope, the same path `ProofIR.fn_contracts` keys by.
    //
    // This test exercises the specific bug class: two same-bare
    // `down(n: Int) -> Int` native-guarded fns, one in a dep module
    // and one at entry. Both classify as `IntCountdownGuarded`, both
    // emit `def down__aux`, and the rewriter pins each one's
    // recursive call to its OWN `FnId` rather than crossing wires.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-module-native-guarded");
    std::fs::create_dir_all(&root).expect("create root");

    // Worker.av: closed-world (not exposed) `down` countdown with a
    // public `run` calling `down(n)` under a `n >= 0` guard so the
    // classifier accepts it as IntCountdownGuarded.
    std::fs::write(
        root.join("Worker.av"),
        "module Worker\n\
         \x20   exposes [run]\n\
         \x20   intent = \"Closed-world native-guarded countdown.\"\n\
         \x20   effects []\n\
         \n\
         fn down(n: Int) -> Int\n\
         \x20   ? \"Countdown to 0.\"\n\
         \x20   match n\n\
         \x20       0 -> 1\n\
         \x20       _ -> down(n - 1)\n\
         \n\
         fn run(n: Int) -> Int\n\
         \x20   ? \"Public entry; guards n >= 0 before down.\"\n\
         \x20   match n < 0\n\
         \x20       true  -> 0\n\
         \x20       false -> down(n)\n",
    )
    .expect("write Worker.av");
    // Entry: same-bare `down` with the SAME body shape so both
    // classify as IntCountdownGuarded. If the rewriter pinned by
    // bare name the entry's `down__aux` would consume Worker.down's
    // FnId (or vice versa) and the rewritten body would call the
    // wrong target.
    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [Worker]\n\
         \x20   intent = \"Same-bare-name native-guarded countdown alongside Worker.down.\"\n\
         \x20   effects []\n\
         \n\
         fn down(n: Int) -> Int\n\
         \x20   ? \"Entry's own countdown — bare-name twin of Worker.down.\"\n\
         \x20   match n\n\
         \x20       0 -> 2\n\
         \x20       _ -> down(n - 1)\n\
         \n\
         fn launch(n: Int) -> Int\n\
         \x20   ? \"Guards n >= 0 before calling Entry.down.\"\n\
         \x20   match n < 0\n\
         \x20       true  -> 0\n\
         \x20       false -> down(n)\n\
         \n\
         fn main() -> Int\n\
         \x20   launch(3) + Worker.run(5)\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed (expected to succeed without FnKey::entry panic):\n{}",
        format_output(&proof)
    );

    let worker_lean = std::fs::read_to_string(out_dir.join("Worker.lean"))
        .expect("read Worker.lean (module-owned native-guarded emit must succeed)");
    // Entry file basename is project-name-derived; the proof exporter
    // capitalises the project name to produce a Lean module ident
    // (`entry.av` → `Entry.lean`). macOS APFS is case-insensitive so a
    // lowercase path would silently match locally — on Linux CI it
    // does not, so look up the canonical capitalised form.
    let entry_lean = std::fs::read_to_string(out_dir.join("Entry.lean")).expect("read Entry.lean");

    // Both modules carry their OWN native-guarded aux def. If the
    // rewriter targeted the wrong FnId only one would emit, or both
    // would inline the same body.
    assert!(
        worker_lean.contains("def down__aux"),
        "Worker.lean must contain its own native-guarded aux def:\n{worker_lean}"
    );
    assert!(
        entry_lean.contains("def down__aux"),
        "entry.lean must contain its own native-guarded aux def:\n{entry_lean}"
    );
    // The hard regression assertion: the rewritten body MUST contain
    // the aux call carrying the `(by omega)` OMEGA_PROOF_SENTINEL
    // tail. With the pre-fix bare-name `FnKey::entry("down")` lookup
    // Worker.down's body would walk past every callsite (the entry
    // FnId never matches Worker.down's resolved `ResolvedCallee::Fn`
    // calls), so the recursive `down(n - 1)` stays unchanged and
    // Lean's termination check loses the precondition handle. Pin
    // both files: Worker AND entry produce the rewritten aux call.
    assert!(
        worker_lean.contains("down__aux (n - 1) (by omega)"),
        "Worker.down__aux body must contain the rewritten recursive call \
         `down__aux (n - 1) (by omega)` — the rewriter dropped it:\n{worker_lean}"
    );
    assert!(
        entry_lean.contains("down__aux (n - 1) (by omega)"),
        "entry.down__aux body must contain the rewritten recursive call \
         `down__aux (n - 1) (by omega)` — the rewriter dropped it:\n{entry_lean}"
    );
    // Worker's base arm is `0 -> 1`; entry's is `0 -> 2`. If the
    // rewriter cross-wired the targets the base literal would leak
    // across files.
    let worker_idx = worker_lean
        .find("def down__aux")
        .expect("down__aux present in Worker.lean");
    let worker_aux = &worker_lean[worker_idx..];
    assert!(
        worker_aux.contains("then 1"),
        "Worker.down__aux must keep its OWN base arm literal (1):\n{worker_aux}"
    );
    let entry_idx = entry_lean
        .find("def down__aux")
        .expect("down__aux present in entry.lean");
    let entry_aux = &entry_lean[entry_idx..];
    assert!(
        entry_aux.contains("then 2"),
        "entry.down__aux must keep its OWN base arm literal (2):\n{entry_aux}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_cross_module_differentiated_recursion_shapes_emit_per_module() {
    // Round-6 finding (audit of round 5): the prior test had both
    // modules use the SAME recursion shape (IntCountdown), so even
    // a buggy scope-naive lookup could "accidentally pass" by
    // returning whichever module's identical contract walked first.
    // This test wires module A's `walker(n)` as IntCountdown and
    // module B's `walker(xs)` as ListStructural — different param
    // types AND different fuel metrics. With scope-naive lookup
    // (`find_fn_contract(ctx, "walker")` → first-walked module's
    // contract) the second module's emit would either use the
    // wrong shape or fall back to `partial def`. With pointer-eq
    // scope resolution each module's `walker` lands its OWN
    // classification.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-cross-module-shapes");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("WalkerA.av"),
        "module WalkerA\n\
         \x20   exposes [walker]\n\
         \x20   intent = \"Int countdown shape.\"\n\
         \x20   effects []\n\
         \n\
         fn walker(n: Int) -> Int\n\
         \x20   ? \"Countdown to 0.\"\n\
         \x20   match n <= 0\n\
         \x20       true -> 0\n\
         \x20       false -> walker(n - 1)\n",
    )
    .expect("write WalkerA.av");
    std::fs::write(
        root.join("WalkerB.av"),
        "module WalkerB\n\
         \x20   exposes [walker]\n\
         \x20   intent = \"List structural shape.\"\n\
         \x20   effects []\n\
         \n\
         fn walker(xs: List<Int>) -> Int\n\
         \x20   ? \"Sum elements.\"\n\
         \x20   match xs\n\
         \x20       [] -> 0\n\
         \x20       [x, ..rest] -> x + walker(rest)\n",
    )
    .expect("write WalkerB.av");
    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [WalkerA, WalkerB]\n\
         \x20   intent = \"Touch both walker modules.\"\n\
         \n\
         fn main() -> Int\n\
         \x20   0\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let a_lean = std::fs::read_to_string(out_dir.join("WalkerA.lean")).expect("read WalkerA.lean");
    let b_lean = std::fs::read_to_string(out_dir.join("WalkerB.lean")).expect("read WalkerB.lean");

    // WalkerA is IntCountdown → fuel-encoded `def walker__fuel
    // (fuel : Nat) (n : Int) : Int`.
    assert!(
        a_lean.contains("def walker__fuel"),
        "WalkerA.walker (IntCountdown) must emit fuel-encoded def:\n{a_lean}"
    );
    assert!(
        a_lean.contains("(n : Int)"),
        "WalkerA.walker fuel sig must carry Int param `n`:\n{a_lean}"
    );

    // WalkerB is ListStructural → backend may emit either a
    // structural-recursion `def walker (xs : List Int)` or a
    // fuel-encoded variant depending on classifier path. Either is
    // fine; the wrong-shape failure mode would be either (a)
    // landing the Int sig from WalkerA, or (b) emitting
    // `partial def walker` because the scope-naive lookup hit the
    // wrong contract and the emit fell through.
    assert!(
        b_lean.contains("walker") && (b_lean.contains("List Int") || b_lean.contains("(xs :")),
        "WalkerB.walker must carry the List<Int> signature, not WalkerA's Int sig:\n{b_lean}"
    );
    assert!(
        !b_lean.contains("partial def walker"),
        "WalkerB.walker must not regress to `partial def` — scope-naive lookup \
         leaked the wrong contract:\n{b_lean}"
    );
    // Defence: WalkerA must not pick up WalkerB's List signature.
    assert!(
        !a_lean.contains("List Int"),
        "WalkerA.walker leaked WalkerB's List signature:\n{a_lean}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_cross_module_refined_types_keep_distinct_predicates() {
    // Review findings 2 + 3 (round 2): two modules each declaring a
    // refined `Natural` (different predicates) must each carry its
    // own predicate into the Lean / Dafny export. Pre-fix
    // `populate_refined_types` keyed `refined_types` by bare name
    // and called the unscoped `refinement_info_for` — both `A` and
    // `B` ended up sharing whichever predicate walked first. The
    // canonical-key + scoped-info path gives each module its own
    // slot; `find_refined_type_scoped` then resolves bare lookups
    // inside each module's emit pass to the local entry.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-cross-module-refined");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("AAA.av"),
        "module AAA\n\
         \x20   exposes [fromInt]\n\
         \x20   exposes opaque [Natural]\n\
         \x20   intent = \"Module AAA's Natural — non-negative.\"\n\
         \x20   effects []\n\
         \n\
         record Natural\n\
         \x20   value: Int\n\
         \n\
         fn fromInt(n: Int) -> Result<Natural, String>\n\
         \x20   ? \"Smart constructor — non-negative.\"\n\
         \x20   match n >= 0\n\
         \x20       true  -> Result.Ok(Natural(value = n))\n\
         \x20       false -> Result.Err(\"AAA: must be non-negative\")\n",
    )
    .expect("write AAA.av");

    std::fs::write(
        root.join("BBB.av"),
        "module BBB\n\
         \x20   exposes [fromInt]\n\
         \x20   exposes opaque [Natural]\n\
         \x20   intent = \"Module BBB's Natural — at least 10.\"\n\
         \x20   effects []\n\
         \n\
         record Natural\n\
         \x20   value: Int\n\
         \n\
         fn fromInt(n: Int) -> Result<Natural, String>\n\
         \x20   ? \"Smart constructor — at least 10.\"\n\
         \x20   match n >= 10\n\
         \x20       true  -> Result.Ok(Natural(value = n))\n\
         \x20       false -> Result.Err(\"BBB: must be >= 10\")\n",
    )
    .expect("write BBB.av");

    std::fs::write(
        root.join("entry.av"),
        "module Entry\n\
         \x20   depends [AAA, BBB]\n\
         \x20   intent = \"Touches both Naturals so both surface in the proof IR.\"\n\
         \n\
         fn main() -> Int\n\
         \x20   0\n",
    )
    .expect("write entry.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let aaa_lean = std::fs::read_to_string(out_dir.join("AAA.lean")).expect("read AAA.lean");
    let bbb_lean = std::fs::read_to_string(out_dir.join("BBB.lean")).expect("read BBB.lean");

    // AAA's Natural carries `>= 0`; BBB's carries `>= 10`. Each
    // module's emit pass must resolve its own predicate, not the
    // other's. Before the scope fix, populate kept only the first
    // walked predicate under bare key `Natural` and both modules
    // emitted the same subtype.
    assert!(
        aaa_lean.contains("abbrev Natural") && aaa_lean.contains(">= 0"),
        "AAA.lean must abbrev Natural with `>= 0`; got:\n{aaa_lean}"
    );
    assert!(
        bbb_lean.contains("abbrev Natural") && bbb_lean.contains(">= 10"),
        "BBB.lean must abbrev Natural with `>= 10`; got:\n{bbb_lean}"
    );
    // Defense in depth: AAA's emit must not carry BBB's predicate
    // or vice versa.
    assert!(
        !aaa_lean.contains(">= 10"),
        "AAA.lean leaked BBB's predicate; got:\n{aaa_lean}"
    );
    assert!(
        !bbb_lean.contains("n >= 0 "),
        "BBB.lean leaked AAA's predicate; got:\n{bbb_lean}"
    );

    // Round-3 finding 1: the prior round only checked Lean. `pick_
    // witness` was un-scoped and tried only `[0, 1, -1]` candidates,
    // so `BBB.Natural`'s `n >= 10` got `witness = None` and Dafny
    // silently fell back to `witness 0` — which violates the
    // predicate. The scoped picker now (a) scopes the smart-ctor
    // walk to the same module and (b) sweeps higher candidates.
    let dafny_out = root.join("out-dafny");
    let dafny_proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("entry.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&dafny_out)
        .output()
        .expect("aver proof --backend dafny");
    assert!(
        dafny_proof.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&dafny_proof)
    );

    let aaa_dfy = std::fs::read_to_string(dafny_out.join("AAA.dfy")).expect("read AAA.dfy");
    let bbb_dfy = std::fs::read_to_string(dafny_out.join("BBB.dfy")).expect("read BBB.dfy");

    assert!(
        aaa_dfy.contains("type Natural") && aaa_dfy.contains("n >= 0"),
        "AAA.dfy must declare `type Natural` with `n >= 0`; got:\n{aaa_dfy}"
    );
    assert!(
        bbb_dfy.contains("type Natural") && bbb_dfy.contains("n >= 10"),
        "BBB.dfy must declare `type Natural` with `n >= 10`; got:\n{bbb_dfy}"
    );
    let bbb_witness_line = bbb_dfy
        .lines()
        .find(|l| l.contains("type Natural"))
        .expect("BBB.dfy must declare `type Natural`");
    assert!(
        !bbb_witness_line.contains("witness 0"),
        "BBB.Natural with `n >= 10` must NOT fall back to `witness 0`; \
         got line:\n{bbb_witness_line}"
    );

    // Round-4 finding 3: text checks aren't enough — the witness
    // must actually satisfy the predicate or Dafny rejects the
    // subset type at verify time. Run `dafny verify` on the
    // generated project to catch unsound witnesses going forward.
    // Skipped silently when dafny isn't on PATH (matches the
    // pattern used by `assert_dafny_verifies`).
    if Command::new("dafny").arg("--version").output().is_ok() {
        let verify = Command::new("dafny")
            .current_dir(&dafny_out)
            .arg("verify")
            .arg("Entry.dfy")
            .output()
            .expect("dafny verify");
        assert!(
            verify.status.success(),
            "`dafny verify` rejected the cross-module refinement output \
             — most likely a witness violates its predicate:\n{}",
            format_output(&verify)
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn embedded_bytes_and_crypto_digest_preserve_refinements_in_both_proof_backends() {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture = repo_root.join("tests/fixtures/stdlib_bytes_app.av");
    let root = temp_output_dir("aver-proof-embedded-stdlib-bytes");
    let missing_module_root = root.join("no-project-modules");

    let lean_out = root.join("lean");
    let mut lean_command = Command::new(aver_bin);
    lean_command
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&fixture)
        .arg("--module-root")
        .arg(&missing_module_root)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&lean_out);
    if Command::new("lake").arg("--version").output().is_ok() {
        lean_command.arg("--check");
    }
    let lean = lean_command
        .output()
        .expect("aver proof embedded Bytes and Crypto.Digest32 with Lean");
    assert!(
        lean.status.success(),
        "Lean rejected embedded Bytes/Crypto.Digest32:\n{}",
        format_output(&lean)
    );
    let bytes_lean =
        std::fs::read_to_string(lean_out.join("Bytes.lean")).expect("read generated Bytes.lean");
    let digest_lean = std::fs::read_to_string(lean_out.join("Crypto/Digest32.lean"))
        .expect("read generated Crypto/Digest32.lean");
    let crypto_lean =
        std::fs::read_to_string(lean_out.join("Crypto.lean")).expect("read generated Crypto.lean");
    // `Bytes` keeps its bare name inside its own module and is spelled by
    // its owner (`Bytes.Bytes`) in `Crypto.Digest32`, like every foreign type.
    assert!(
        bytes_lean.contains("abbrev Bytes := { xs : List Int // Bytes.allInRange xs }")
            && bytes_lean.contains("def stringToUtf8 (s : String) : Bytes")
            && bytes_lean.contains("def stringFromUtf8 (bytes : Bytes) : Except String String")
            && !bytes_lean.contains("def fromUtf8String")
            && !bytes_lean.contains("def toUtf8String")
            && digest_lean.contains(
                "abbrev Digest32 := { payload : Bytes.Bytes // Crypto.Digest32.hasLength32 payload }"
            ),
        "embedded standard refinements degraded in Lean:\n{bytes_lean}\n{digest_lean}"
    );
    assert!(
        crypto_lean.contains("def sha256 (bytes : Bytes.Bytes)")
            && crypto_lean.contains("def compress")
            && !crypto_lean.contains("axiom ")
            && !crypto_lean.contains("sorry")
            && !crypto_lean.contains("native_decide")
            && !crypto_lean.contains("while ")
            && !crypto_lean.contains("partial "),
        "Lean Crypto.sha256 must be an executable, axiom-free, kernel-reducible model \
         (no native_decide, no `while` loops bottoming out in partial combinators):\n{crypto_lean}"
    );

    let dafny_out = root.join("dafny");
    let mut dafny_command = Command::new(aver_bin);
    dafny_command
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&fixture)
        .arg("--module-root")
        .arg(&missing_module_root)
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&dafny_out);
    if Command::new("dafny").arg("--version").output().is_ok() {
        dafny_command.arg("--check");
    }
    let dafny = dafny_command
        .output()
        .expect("aver proof embedded Bytes and Crypto.Digest32 with Dafny");
    assert!(
        dafny.status.success(),
        "Dafny rejected embedded Bytes/Crypto.Digest32:\n{}",
        format_output(&dafny)
    );
    let bytes_dafny =
        std::fs::read_to_string(dafny_out.join("Bytes.dfy")).expect("read generated Bytes.dfy");
    let digest_dafny = std::fs::read_to_string(dafny_out.join("Crypto/Digest32.dfy"))
        .expect("read generated Crypto/Digest32.dfy");
    let crypto_dafny =
        std::fs::read_to_string(dafny_out.join("Crypto.dfy")).expect("read generated Crypto.dfy");
    let crypto_core_dafny = std::fs::read_to_string(dafny_out.join("Crypto/Sha256Core.dfy"))
        .expect("read generated Crypto/Sha256Core.dfy");
    assert!(
        bytes_dafny.contains("type Bytes = xs: seq<int> | allInRange(xs) witness *")
            && digest_dafny
                .contains("type Digest32 = payload: Bytes | hasLength32(payload) witness *"),
        "embedded standard refinements degraded in Dafny:\n{bytes_dafny}\n{digest_dafny}"
    );
    assert!(
        crypto_dafny.contains("function sha256(bytes: Bytes): Digest32")
            && crypto_core_dafny.contains("function compress")
            && !crypto_dafny.contains("assume")
            && !crypto_dafny.contains("{:axiom}")
            && !crypto_core_dafny.contains("assume")
            && !crypto_core_dafny.contains("{:axiom}"),
        "Dafny Crypto.sha256 must be an executable, axiom-free model:\n{crypto_dafny}\n{crypto_core_dafny}"
    );
    if Command::new("dafny").arg("--version").output().is_ok() {
        let vectors = r#"include "Crypto/Sha256Core.dfy"

module CryptoVectorCheck {
  import opened Aver_Crypto_Sha256Core

  method Main() {
    expect sha256Raw([]) ==
      [227, 176, 196, 66, 152, 252, 28, 20, 154, 251, 244, 200, 153, 111, 185, 36,
       39, 174, 65, 228, 100, 155, 147, 76, 164, 149, 153, 27, 120, 82, 184, 85];
    expect sha256Raw([97, 98, 99]) ==
      [186, 120, 22, 191, 143, 1, 207, 234, 65, 65, 64, 222, 93, 174, 34, 35,
       176, 3, 97, 163, 150, 23, 122, 156, 180, 16, 255, 97, 242, 0, 21, 173];
    expect sha256Raw(
      [97, 98, 99, 100, 98, 99, 100, 101, 99, 100, 101, 102, 100, 101, 102, 103,
       101, 102, 103, 104, 102, 103, 104, 105, 103, 104, 105, 106, 104, 105, 106, 107,
       105, 106, 107, 108, 106, 107, 108, 109, 107, 108, 109, 110, 108, 109, 110, 111,
       109, 110, 111, 112, 110, 111, 112, 113]) ==
      [36, 141, 106, 97, 210, 6, 56, 184, 229, 192, 38, 147, 12, 62, 96, 57,
       163, 60, 228, 89, 100, 255, 33, 103, 246, 236, 237, 212, 25, 219, 6, 193];
  }
}
"#;
        std::fs::write(dafny_out.join("CryptoVectors.dfy"), vectors)
            .expect("write Dafny SHA-256 vector driver");
        let run = Command::new("dafny")
            .current_dir(&dafny_out)
            .arg("run")
            .arg("CryptoVectors.dfy")
            .arg("--no-verify")
            .output()
            .expect("run executable Dafny SHA-256 vectors");
        assert!(
            run.status.success(),
            "Dafny Crypto.sha256 disagrees with FIPS vectors:\n{}",
            format_output(&run)
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_preserves_container_and_nested_refinements_end_to_end() {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture = repo_root.join("tests/fixtures/refinement_container_nested.av");
    let root = temp_output_dir("aver-proof-container-nested-refinement");

    let lean_out = root.join("lean");
    let mut lean_command = Command::new(aver_bin);
    lean_command
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&fixture)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&lean_out);
    if Command::new("lake").arg("--version").output().is_ok() {
        lean_command.arg("--check");
    }
    let lean = lean_command
        .output()
        .expect("aver proof --backend lean for container refinements");
    assert!(
        lean.status.success(),
        "Lean rejected container/nested refinement export:\n{}",
        format_output(&lean)
    );

    let lean_src = std::fs::read_to_string(lean_out.join("ContainerRefinement.lean"))
        .expect("read ContainerRefinement.lean");
    let all_in_range = lean_src
        .find("def allInRange")
        .expect("named container predicate must be emitted");
    let bytes_type = lean_src
        .find("abbrev Bytes")
        .expect("Bytes must emit as a Lean Subtype");
    let to_list = lean_src
        .find("def toList")
        .expect("carrier projection must be emitted");
    let has_length = lean_src
        .find("def hasLength32")
        .expect("named nested predicate must be emitted");
    let digest_type = lean_src
        .find("abbrev Digest32")
        .expect("Digest32 must emit as a nested Lean Subtype");

    assert!(
        all_in_range < bytes_type,
        "Bytes' predicate must be declared before its Subtype:\n{lean_src}"
    );
    assert!(
        bytes_type < to_list && to_list < has_length && has_length < digest_type,
        "nested refinement dependencies must be emitted in declaration order:\n{lean_src}"
    );
    assert!(
        lean_src.contains("abbrev Bytes := { xs : List Int // allInRange xs }")
            && lean_src.contains("abbrev Digest32 := { bytes : Bytes // hasLength32 bytes }"),
        "both invariants must ride in the generated Lean types:\n{lean_src}"
    );
    assert!(
        !lean_src.contains("structure Bytes where")
            && !lean_src.contains("structure Digest32 where"),
        "refinements must never silently degrade to plain structures:\n{lean_src}"
    );

    let dafny_out = root.join("dafny");
    let mut dafny_command = Command::new(aver_bin);
    dafny_command
        .current_dir(&repo_root)
        .arg("proof")
        .arg(&fixture)
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&dafny_out);
    if Command::new("dafny").arg("--version").output().is_ok() {
        dafny_command.arg("--check");
    }
    let dafny = dafny_command
        .output()
        .expect("aver proof --backend dafny for container refinements");
    assert!(
        dafny.status.success(),
        "Dafny rejected container/nested refinement export:\n{}",
        format_output(&dafny)
    );

    let dafny_src = std::fs::read_to_string(dafny_out.join("ContainerRefinement.dfy"))
        .expect("read ContainerRefinement.dfy");
    assert!(
        dafny_src.contains("type Bytes = xs: seq<int> | allInRange(xs) witness *")
            && dafny_src.contains("type Digest32 = bytes: Bytes | hasLength32(bytes) witness *"),
        "both invariants must ride in the generated Dafny subset types:\n{dafny_src}"
    );
    assert!(
        !dafny_src.contains("datatype Bytes = Bytes(")
            && !dafny_src.contains("datatype Digest32 = Digest32("),
        "refinements must never silently degrade to plain datatypes:\n{dafny_src}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_lake_builds_red_black_tree_after_singleton_and_fuel_gates() {
    // Issue #128: red_black_tree.av carried 44 lake errors after the
    // #123 path-shadow / `.val` fixes. The diagnosis in the issue
    // text (anonymous `{...}` constructor notation) didn't match the
    // real output — match arms already used qualified positional
    // syntax. The actual failure was two coupled emit shapes:
    //
    //   1. Laws with singleton-domain givens and a RHS that didn't
    //      reference any given (`checkRight L V R = Tree.Black Empty
    //      1 Empty`) emit a `∀ L V R, …` universal that's vacuous or
    //      outright false. The `induction L with …` fallback chose
    //      by the auto-proof matcher then failed to close.
    //   2. Laws calling fuel-bounded fns that the proof-mode
    //      classifier rejected (`size`, `valuesInOrder`) emit
    //      `induction t with …` against `__fuel`-wrapped helpers
    //      whose recursive shape `simp` can't drive.
    //
    // Both gated at the universal emit step; sample / checked_domain
    // lemmas remain (concrete inputs stay decidable). Lake build
    // succeeds; `aver verify` runtime hits every declared case.
    //
    // Sorry budget 1 (was 2): the `detect.rs` resolved-subject fix (which lets
    // Dafny/Z3 prove the Peano-fold homomorphism family) also admits
    // `size` / `valuesInOrder`'s structural recursion into the proof subset, so
    // their two universals EMIT a `∀ … induction t with …` proof rather than
    // gating to sample-only. On Lean those can't close on the ladder — the
    // `__fuel`-wrapped recursion needs a fuel-saturation lemma the auto
    // template lacks.
    //
    // The drop 2→1 is the discovery feedback loop, część A: `valuesInOrder_law_
    // sizePreserved` now closes its TACTIC BLOCK via the fast path `simp only
    // [size_law_equalsSortedLen] <;> omega`, referencing the earlier sibling
    // theorem — so it no longer emits its OWN `sorry`. This is a textual-count
    // drop only, NOT a new genuine universal: `size_law_equalsSortedLen` still
    // `sorry`s, so the consumer inherits `sorryAx` and the `universal` metric
    // correctly stays false for both (verified via `#print axioms`). The
    // honest coverage number is unchanged; only the weaker sorry-count metric
    // moved. Z3 supplies the missing induction automatically, which is why the
    // same laws DO prove on the Dafny backend.
    assert_proof_builds_with_sorry_budget(
        "examples/data/red_black_tree.av",
        "aver-proof-red-black-tree",
        1,
    );
}

#[test]
fn proof_export_gates_trace_projection_law_lhs_as_runtime_only() {
    // Issue #127: a `verify fn trace law` whose LHS projects through
    // `.trace.{event,group,branch}` references the runtime trace
    // buffer, not the lifted fn's return. The lifted Lean / Dafny fn
    // has no `.trace` field — emitting `fn().trace.event 0` as a
    // theorem (universal or sample) produces invalid-field-notation
    // errors. Backends now emit a `runtime-only` comment instead and
    // skip the universal/sample theorem. The `aver verify` runtime
    // path still exercises the law under its stubs.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let lean_dir = temp_output_dir("aver-proof-issue127-lean");
    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/formal/hostile_order_axis.av")
        .arg("-o")
        .arg(&lean_dir)
        .output()
        .expect("expected `aver proof` to run");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let entry_text = std::fs::read_to_string(lean_dir.join("HostileOrderAxis.lean"))
        .expect("read HostileOrderAxis.lean");

    assert!(
        entry_text.contains(
            "-- verify law rollPair.firstEventOfFirstBranch: \
             trace-projection LHS is runtime-only"
        ),
        "expected runtime-only gate marker for firstEventOfFirstBranch in \
         entry Lean; got:\n{entry_text}"
    );
    assert!(
        entry_text.contains(
            "-- verify law rollPair.firstEventOfSecondBranch: \
             trace-projection LHS is runtime-only"
        ),
        "expected runtime-only gate marker for firstEventOfSecondBranch in \
         entry Lean; got:\n{entry_text}"
    );
    // Defense in depth: the universal/sample theorems must not slip
    // back in — their LHS triggers Lean's invalid-field-notation
    // diagnostic on the bare `(Int × Int)` return.
    assert!(
        !entry_text.contains("rollPair_law_firstEventOfFirstBranch"),
        "universal theorem leaked through the trace-projection gate; \
         got:\n{entry_text}"
    );
    assert!(
        !entry_text.contains(").event 0"),
        "trace projection chain leaked into elaborated Lean; got:\n{entry_text}"
    );
    assert!(
        !entry_text.contains("EffectEvent"),
        "EffectEvent literal leaked into elaborated Lean (gate should \
         keep it out entirely); got:\n{entry_text}"
    );

    let _ = std::fs::remove_dir_all(&lean_dir);

    let dafny_dir = temp_output_dir("aver-proof-issue127-dafny");
    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/formal/hostile_order_axis.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&dafny_dir)
        .output()
        .expect("expected `aver proof --backend dafny` to run");
    assert!(
        proof.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&proof)
    );

    let dafny_entry = std::fs::read_to_string(dafny_dir.join("HostileOrderAxis.dfy"))
        .expect("read HostileOrderAxis.dfy");

    assert!(
        dafny_entry.contains(
            "// Law rollPair.firstEventOfFirstBranch: trace-projection LHS is runtime-only"
        ),
        "expected Dafny runtime-only gate marker; got:\n{dafny_entry}"
    );
    assert!(
        !dafny_entry.contains("lemma {:fuel rollPair, 5} rollPair_firstEventOfFirstBranch"),
        "universal lemma leaked through the trace-projection gate; got:\n{dafny_entry}"
    );
    assert!(
        !dafny_entry.contains(".trace.group"),
        "trace projection chain leaked into elaborated Dafny; got:\n{dafny_entry}"
    );
    assert!(
        !dafny_entry.contains("EffectEvent"),
        "EffectEvent literal leaked into elaborated Dafny (gate should \
         keep it out entirely); got:\n{dafny_entry}"
    );

    let _ = std::fs::remove_dir_all(&dafny_dir);
}

#[test]
fn proof_export_lean_chunks_large_checked_domain_conjunction() {
    // Emission-shape half of the large-domain fix (the live build/credit
    // half lives in `check_gates`): a 512-cell given product must emit
    // its checked-domain conjunction as `_checked_domain_part<N>`
    // theorems of at most 32 conjuncts each — one 512-conjunct theorem
    // exceeds the elaborator's recursion depth during `Decidable`
    // synthesis and the whole file fails to build. Fast (no lake).
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = temp_output_dir("aver-large-domain-shape-out");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/large_domain_law.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(out.join("LargeDomainLaw.lean"))
        .expect("read emitted LargeDomainLaw.lean");
    assert!(
        !lean.contains("theorem tripleSum_law_mirror_checked_domain :"),
        "512 conjuncts must not emit as a single checked-domain theorem; got:\n{}",
        lean.lines().take(40).collect::<Vec<_>>().join("\n")
    );
    // 512 cases / 32-conjunct chunks = 16 part theorems, all proved.
    for part in 1..=16 {
        assert!(
            lean.contains(&format!(
                "theorem tripleSum_law_mirror_checked_domain_part{} :",
                part
            )),
            "missing checked-domain part theorem {part}"
        );
    }
    assert!(
        !lean.contains("tripleSum_law_mirror_checked_domain_part17"),
        "expected exactly 16 part theorems"
    );
    for line in lean.lines() {
        if line.contains("_checked_domain_part") {
            let conjuncts = line.matches(" ∧ ").count() + 1;
            assert!(
                conjuncts <= 32,
                "part theorem exceeds the 32-conjunct chunk bound ({conjuncts}):\n{line}"
            );
        }
    }
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_export_lean_chunks_large_checked_domain_in_every_verify_mode() {
    // Mode-parameterized twin of the chunking pin above. The
    // `maxRecDepth` wall is NOT specific to `native_decide`'s
    // `Decidable` synthesis: plain elaboration of the nested-∧
    // STATEMENT recurses once per conjunct, so a 512-conjunct theorem
    // fails the build identically under `--verify-mode sorry` and
    // `--verify-mode theorem-skeleton` (the proof body is never
    // reached). Every mode must emit the same 16-part partition, each
    // part carrying the mode's own proof shape. Fast (no lake; the
    // sorry / theorem-skeleton exports were lake-verified by hand when
    // the chunking was extended).
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // (mode flag, the proof shape every part theorem must carry)
    let modes: [(Option<&str>, &str); 3] = [
        (None, ":= by native_decide"),
        (Some("sorry"), ":= by sorry"),
        (Some("theorem-skeleton"), ":= by\n  sorry"),
    ];
    for (mode, proof_shape) in modes {
        let out = temp_output_dir("aver-large-domain-mode-shape-out");
        let mut cmd = Command::new(aver_bin);
        cmd.current_dir(&repo_root)
            .arg("proof")
            .arg("tests/fixtures/large_domain_law.av")
            .arg("--backend")
            .arg("lean")
            .arg("-o")
            .arg(&out);
        if let Some(mode) = mode {
            cmd.arg("--verify-mode").arg(mode);
        }
        let run = cmd.output().expect("expected `aver proof` to run");
        assert!(
            run.status.success(),
            "mode {mode:?}: {}",
            format_output(&run)
        );
        let lean = std::fs::read_to_string(out.join("LargeDomainLaw.lean"))
            .expect("read emitted LargeDomainLaw.lean");
        assert!(
            !lean.contains("theorem tripleSum_law_mirror_checked_domain :"),
            "mode {mode:?}: 512 conjuncts must not emit as a single \
             checked-domain theorem"
        );
        for part in 1..=16 {
            let header = format!("theorem tripleSum_law_mirror_checked_domain_part{part} :");
            let Some(at) = lean.find(&header) else {
                panic!("mode {mode:?}: missing checked-domain part theorem {part}");
            };
            assert!(
                lean[at..at + 16_384.min(lean.len() - at)].contains(proof_shape),
                "mode {mode:?}: part theorem {part} must carry the mode's \
                 proof shape `{proof_shape}`"
            );
        }
        assert!(
            !lean.contains("tripleSum_law_mirror_checked_domain_part17"),
            "mode {mode:?}: expected exactly 16 part theorems"
        );
        let _ = std::fs::remove_dir_all(&out);
    }
}

#[test]
fn proof_export_lean_at_edge_domain_is_byte_identical_to_baseline() {
    // No-movement guarantee for the bounded-law partitioning: a fixture
    // whose largest given (8 values) is below the 128-value edge and
    // whose case product (8x8x8 = 512) sits exactly at the 512-case edge
    // must emit byte-for-byte the same `LargeDomainLaw.lean` the
    // pre-partitioning emitter did. The golden snapshot
    // (`tests/fixtures/large_domain_law.baseline.lean`) preserves that
    // pre-partitioning output inside the entry module namespace; any other
    // drift means the partitioning moved an at/below-edge export. Fast (no lake).
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out = temp_output_dir("aver-large-domain-byte-identity");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/large_domain_law.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(run.status.success(), "{}", format_output(&run));
    let emitted =
        std::fs::read_to_string(out.join("LargeDomainLaw.lean")).expect("read emitted Lean");
    let baseline_file =
        std::fs::read_to_string(repo_root.join("tests/fixtures/large_domain_law.baseline.lean"))
            .expect("read baseline golden Lean");
    let baseline = baseline_file.strip_suffix('\n').unwrap_or(&baseline_file);
    assert_eq!(
        emitted, baseline,
        "at/below-edge export must stay byte-identical to the pre-partitioning \
         baseline; the partitioning must not move sub-edge emission"
    );
    let _ = std::fs::remove_dir_all(&out);
}

#[test]
fn proof_export_rational_ring_laws_carry_ac_ring_package() {
    // Prover-free pin of the `RingIdentity` emission on the
    // exact-rationals corpus example: every one of the ten law
    // theorems must render the strategy's `first | (simp [<cone>,
    // <AC-ring package>]; done) | sorry` rung. Reverting the strategy
    // drops the laws back to the prelude-simp rung (`Int.add_sub_cancel`
    // set, no AC normalization), which this catches without lake; the
    // live closure itself (0 sorries, kernel-genuine) is pinned by
    // `builds::proof_export_builds_rational_ring_laws_kernel_genuine_*`.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let lean_dir = temp_output_dir("aver-proof-rational-ring-export");
    let proof = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("examples/data/rational.av")
        .arg("-o")
        .arg(&lean_dir)
        .output()
        .expect("expected `aver proof` to run");
    assert!(
        proof.status.success(),
        "`aver proof` failed:\n{}",
        format_output(&proof)
    );

    let entry_text =
        std::fs::read_to_string(lean_dir.join("Rational.lean")).expect("read Rational.lean");

    // One AC-normalizing tactic per law — the package's permutational
    // core (`Int.mul_left_comm` / `Int.add_left_comm`) only ever
    // appears in this strategy's emission, so the count is exact.
    let package_tactics = entry_text.matches("Int.mul_left_comm").count();
    assert_eq!(
        package_tactics, 10,
        "all ten ring laws must render the AC-ring package; got {package_tactics} in:\n{entry_text}"
    );
    assert_eq!(
        entry_text.matches("Int.add_left_comm").count(),
        10,
        "the additive AC triple must ride along in every emission"
    );
    // The honest floor stays: the rung is
    // `first | (grind [<cone>]; done) | (simp …; done) | sorry`, never a
    // bare tactic that could surface a build error. `grind [<cone>]`
    // leads (Lean 4.31 closes the nonlinear ring identities the
    // AC-ring simp package stopped normalizing); the simp package and
    // the caught `sorry` remain as the lower rungs.
    assert_eq!(
        entry_text.matches("first | (grind [").count(),
        10,
        "every ring law must lead with the cone-aware grind rung"
    );
    assert_eq!(
        entry_text.matches("; done) | (simp [").count(),
        10,
        "every ring law must keep the simp package and caught-sorry alternation"
    );

    let _ = std::fs::remove_dir_all(&lean_dir);
}

#[test]
fn proof_export_dafny_routes_nested_error_prop_to_an_axiom() {
    // #825: the pure `?` lowering rewrites `?` at statement position
    // into a `match` cascade, but recurses through a `?` nested inside
    // an expression and leaves it there. The Dafny axiom gate used to
    // ask whether the lowering had produced anything at all, so a
    // nested `?` read as "handled" and fell through to ordinary
    // emission — where `emit_expr` rendered it as the error marker it
    // documents as unreachable, and `dafny verify` rejected the file
    // with an arity error naming the innocent helper.
    //
    // `stepA`/`stepB` cover the second half: a residual `?` inside a
    // mutual-recursion SCC must be excluded from the fuel group, or it
    // gets a fuel definition AND an axiom — the same function declared
    // twice.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = temp_output_dir("aver-proof-dafny-nested-error-prop");
    std::fs::create_dir_all(&root).expect("create root");

    std::fs::write(
        root.join("nested.av"),
        "module NestedErrorProp\n\
         \x20   effects []\n\
         \x20   intent = \"Nested `?`, plain and inside a mutual SCC.\"\n\
         \n\
         fn addOne(x: Int) -> Int\n\
         \x20   ? \"Add one.\"\n\
         \x20   x + 1\n\
         \n\
         fn nestedArg(n: Int, d: Int) -> Result<Int, String>\n\
         \x20   ? \"`?` nested as a call argument.\"\n\
         \x20   Result.Ok(addOne(Int.div(n, d)?))\n\
         \n\
         fn stepA(n: Int) -> Result<Int, String>\n\
         \x20   ? \"Counts down to its peer, nesting `?` as a call argument.\"\n\
         \x20   match n\n\
         \x20       0 -> Result.Ok(0)\n\
         \x20       _ -> Result.Ok(addOne(stepB(n - 1)?))\n\
         \n\
         fn stepB(n: Int) -> Result<Int, String>\n\
         \x20   ? \"Counts down to its peer.\"\n\
         \x20   match n\n\
         \x20       0 -> Result.Ok(1)\n\
         \x20       _ -> stepA(n - 1)\n",
    )
    .expect("write nested.av");

    let out_dir = root.join("out");
    let proof = Command::new(aver_bin)
        .current_dir(&root)
        .arg("proof")
        .arg("nested.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof --backend dafny");
    assert!(
        proof.status.success(),
        "`aver proof --backend dafny` failed:\n{}",
        format_output(&proof)
    );

    let dfy = std::fs::read_to_string(out_dir.join("NestedErrorProp.dfy"))
        .expect("read NestedErrorProp.dfy");

    // Both halves are asserted: either one alone can pass while the
    // other regresses (a marker-free body, or an axiom that still
    // carries the marker in a sibling).
    assert!(
        dfy.contains("function {:axiom} nestedArg("),
        "a fn with a nested `?` must export as an axiom, not a body:\n{dfy}"
    );
    assert!(
        !dfy.contains("ERROR:"),
        "the emitted Dafny must carry no error marker:\n{dfy}"
    );

    assert!(
        dfy.contains("function {:axiom} stepA("),
        "the fuel-recursive fn with a nested `?` must export as an axiom:\n{dfy}"
    );
    assert!(
        !dfy.contains("stepA__fuel"),
        "the axiomized fn must not also get a fuel definition:\n{dfy}"
    );
    let step_a_decls = dfy
        .lines()
        .filter(|line| line.starts_with("function") && line.contains("stepA("))
        .count();
    assert_eq!(
        step_a_decls, 1,
        "stepA must be declared exactly once, got {step_a_decls}:\n{dfy}"
    );
    // The SCC peer keeps its fuel encoding — exclusion is per-fn, not
    // per-group.
    assert!(
        dfy.contains("function stepB__fuel("),
        "the peer without a residual `?` must keep its fuel definition:\n{dfy}"
    );

    if Command::new("dafny").arg("--version").output().is_ok() {
        let verify = Command::new("dafny")
            .current_dir(&out_dir)
            .arg("verify")
            .arg("NestedErrorProp.dfy")
            .output()
            .expect("dafny verify");
        assert!(
            verify.status.success(),
            "`dafny verify` rejected the export:\n{}",
            format_output(&verify)
        );
    } else {
        eprintln!("skipping `dafny verify`: `dafny` not available");
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_of_a_set_shaped_map_builds() {
    // Issue #884: a `Map<T, Unit>` — the set-shaped map — exported Lean that
    // did not build. Three special cases fired on it: the type rendered as
    // `Finset T` (Mathlib, which the generated project does not depend on),
    // `Map.set(s, k, Unit)` emitted `AverSet.add s k`, and a literal emitted
    // `AverSet.ofList [...]`. Nothing anywhere defined `AverSet`, so `lake
    // build` reported `Unknown identifier 'AverSet.add'` and friends while
    // plain `aver proof` still reported success.
    //
    // A fourth thing was queued behind those: the literal branch ran BEFORE
    // the key-ordering branch added for #874, so a set literal written out of
    // key order would have emitted unsorted even with a carrier that existed —
    // a different value from the same set built with `Map.set`. The fixture's
    // literal is written `{"b" => Unit, "a" => Unit}` to hold that down.
    //
    // All three special cases are gone; the set-shaped map goes through the
    // ordinary `AverMap` path, which is defined, key-sorted, and has the `len`
    // the same program reaches for.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = temp_output_dir("aver-proof-set-shaped-map");

    let mut command = Command::new(aver_bin);
    command
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/set_shaped_map.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&root);
    let lake = Command::new("lake").arg("--version").output().is_ok();
    if lake {
        command
            .arg("--check")
            .arg("--check-json")
            .arg("--sorry-budget")
            .arg("0");
    } else {
        eprintln!("skipping the lake build: `lake` not available");
    }
    let output = command.output().expect("aver proof for a set-shaped map");
    let lean =
        std::fs::read_to_string(root.join("SetShapedMap.lean")).expect("read SetShapedMap.lean");

    // The names that did not exist. `Finset` is Mathlib; `AverSet` was never
    // written down at all.
    for absent in ["AverSet", "Finset"] {
        assert!(
            !lean.contains(absent),
            "the export must not mention `{absent}` — nothing in the generated \
             project defines it:\n{lean}"
        );
    }
    // The fixture only tests the set shape while it still holds one, and the
    // literal only tests the ordering while it is still written out of order.
    assert!(
        lean.contains("def addKey (s : List (String × Unit)) (k : String)"),
        "a set-shaped map must render as an ordinary map:\n{lean}"
    );
    assert!(
        lean.contains(
            r#"def seed  : List (String × Unit) :=
  [("a", ()), ("b", ())]"#
        ),
        "a set literal written out of key order must be emitted in key order, \
         or it is a different value from the same set built with `Map.set`:\n{lean}"
    );

    if lake {
        let json_line = String::from_utf8_lossy(&output.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .map(str::to_owned)
            .unwrap_or_else(|| {
                panic!(
                    "`aver proof --check --check-json` produced no JSON line:\n{}",
                    format_output(&output)
                )
            });
        let summary: serde_json::Value =
            serde_json::from_str(&json_line).expect("summary line parses as JSON");
        assert_eq!(
            summary["build_errors"].as_u64(),
            Some(0),
            "the exported set-shaped map must build:\n{}",
            format_output(&output)
        );
        assert_eq!(
            summary["passed"].as_bool(),
            Some(true),
            "the exported set-shaped map must build within a zero sorry budget:\n{}",
            format_output(&output)
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_two_cons_peel_is_structural_not_partial() {
    // A list walked TWO cells per step. The inner `match afterFirst` binds
    // `rest` as a cons-tail of `afterFirst`, which is itself a cons-tail of
    // the parameter `xs` — so `rest` is a tail of a tail, and the recursion
    // is as structurally terminating as a one-cell peel. While the tail-binder
    // collection only recorded binders from a match whose subject was
    // LITERALLY the parameter, `rest` never qualified and the whole fn fell
    // out of the proof subset: Lean got a `partial def` (opaque to the kernel)
    // and every verify case over it was pushed to `native_decide`.
    //
    // Both halves are pinned here because they fail independently: the
    // emitted definition (total `def` + `termination_by xs.length`) and the
    // per-case tactic routing that follows from it.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = temp_output_dir("aver-proof-two-cons-peel");

    let mut command = Command::new(aver_bin);
    command
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/two_cons_peel.av")
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&root);
    let lake = Command::new("lake").arg("--version").output().is_ok();
    if lake {
        command.arg("--check");
    } else {
        eprintln!("skipping the lake build: `lake` not available");
    }
    let output = command
        .output()
        .expect("aver proof --backend lean for the two-cons peel");
    assert!(
        output.status.success(),
        "proof export failed:\n{}",
        format_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("'pairSums' is outside proof subset"),
        "a two-cell peel is structural recursion — it must not be reported \
         outside the proof subset:\n{stderr}"
    );

    let lean =
        std::fs::read_to_string(root.join("TwoConsPeel.lean")).expect("read TwoConsPeel.lean");
    assert!(
        !lean.contains("partial def pairSums"),
        "a two-cell peel must not fall back to `partial def`:\n{lean}"
    );
    assert!(
        lean.contains("def pairSums (xs : List Int) (acc : List Int)"),
        "expected a total `def` for the two-cell peel:\n{lean}"
    );
    assert!(
        lean.contains("termination_by xs.length"),
        "the structural emission must state the list-length measure:\n{lean}"
    );
    for line in lean.lines().filter(|l| l.starts_with("example : ")) {
        assert!(
            line.ends_with(":= by decide +kernel"),
            "with `pairSums` total, its verify cases reduce in the kernel:\n{line}"
        );
    }

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn proof_export_two_cons_peel_gets_a_dafny_decreases_not_an_axiom() {
    // Dafny mirror of the peel: `decreases |xs|` rather than the opaque
    // `function {:axiom}` fallback, so a Dafny consumer can still unfold the
    // parser. Verified by running `dafny verify` when the toolchain is on
    // PATH; without it, the emitted clause is what is asserted.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = temp_output_dir("aver-proof-two-cons-peel-dafny");
    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("tests/fixtures/two_cons_peel.av")
        .arg("--backend")
        .arg("dafny")
        .arg("-o")
        .arg(&root)
        .output()
        .expect("aver proof --backend dafny for the two-cons peel");
    assert!(
        output.status.success(),
        "proof export failed:\n{}",
        format_output(&output)
    );
    let dafny =
        std::fs::read_to_string(root.join("TwoConsPeel.dfy")).expect("read TwoConsPeel.dfy");
    assert!(
        !dafny.contains("function {:axiom} pairSums"),
        "a two-cell peel must not export as an opaque Dafny axiom:\n{dafny}"
    );
    assert!(
        dafny
            .contains("function pairSums(xs: seq<int>, acc: seq<int>): seq<int>\n  decreases |xs|"),
        "expected `decreases |xs|` on the two-cell peel:\n{dafny}"
    );

    if Command::new("dafny").arg("--version").output().is_ok() {
        let verify = Command::new("dafny")
            .current_dir(&root)
            .arg("verify")
            .arg("TwoConsPeel.dfy")
            .output()
            .expect("dafny verify for the two-cons peel");
        assert!(
            verify.status.success(),
            "Dafny rejected the emitted termination measure:\n{}",
            format_output(&verify)
        );
    } else {
        eprintln!("skipping `dafny verify`: `dafny` not available");
    }

    let _ = std::fs::remove_dir_all(&root);
}
