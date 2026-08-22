use super::*;

fn summary_from(output: &std::process::Output) -> serde_json::Value {
    let line = output
        .stdout
        .split(|byte| *byte == b'\n')
        .rev()
        .find_map(|line| {
            std::str::from_utf8(line)
                .ok()
                .filter(|text| text.starts_with('{'))
        })
        .unwrap_or_else(|| panic!("no JSON summary:\n{}", format_output(output)));
    serde_json::from_str(line).unwrap_or_else(|error| {
        panic!(
            "invalid JSON summary ({error}): {line}\n{}",
            format_output(output)
        )
    })
}

fn run_checked_proof(
    source_dir: &std::path::Path,
    output_dir: &std::path::Path,
) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(source_dir)
        .args([
            "proof",
            "main.av",
            "--backend",
            "lean",
            "--verify-mode",
            "auto",
            "--module-root",
            ".",
            "-o",
        ])
        .arg(output_dir)
        .args(["--check", "--check-json"])
        .output()
        .expect("run checked Lean proof export")
}

#[test]
fn pure_bytes_capability_is_noncomputable_and_its_default_matching_case_is_declined() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability opacity proof test: `lake` not available");
        return;
    }

    let source_dir = temp_output_dir("aver-capability-opaque-bytes-src");
    let output_dir = temp_output_dir("aver-capability-opaque-bytes-out");
    std::fs::create_dir_all(&source_dir).expect("create capability source dir");
    std::fs::copy(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("stdlib/bytes.av"),
        source_dir.join("bytes.av"),
    )
    .expect("copy Bytes module");
    std::fs::write(
        source_dir.join("cap.av"),
        r#"module Cap
    intent = "A pure provider-owned hash boundary."
    kind = capability
    semantics = pure
    exposes [hash]
    depends [Bytes]
    effects []

operation hash(input: Bytes) -> Bytes
    ? "Hash bytes through the provider."
"#,
    )
    .expect("write capability module");
    std::fs::write(
        source_dir.join("main.av"),
        r#"module Main
    intent = "Keep pure proofs independent from a provider call cone."
    depends [Bytes, Cap]
    exposes [hashed, twice]
    effects []

fn hashed(values: List<Int>) -> List<Int>
    ? "Validate, hash, and unwrap bytes."
    match Bytes.fromList(values)
        Result.Err(_) -> []
        Result.Ok(raw) -> Bytes.toList(Cap.hash(raw))

fn twice(n: Int) -> Int
    n + n

verify hashed
    hashed([]) => []

verify twice
    twice(2) => 4

verify twice law doubling
    given n: Int = 0..2
    twice(n) => n * 2
"#,
    )
    .expect("write consumer module");

    let proof = run_checked_proof(&source_dir, &output_dir);
    let summary = summary_from(&proof);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(1), Some(0)),
        "the provider case must be declined after a clean Lake build:\n{}",
        format_output(&proof)
    );
    let declined = summary["declined_claims"]
        .as_array()
        .expect("declined_claims array");
    assert_eq!(declined.len(), 1, "{declined:?}");
    assert_eq!(declined[0]["claim"], "hashed");
    assert_eq!(declined[0]["kind"], "cases");
    assert!(
        declined[0]["reason"]
            .as_str()
            .is_some_and(|reason| reason.contains("Cap.hash") && reason.contains("noncomputable")),
        "the structured refusal must name the provider operation: {declined:?}"
    );

    let bytes = std::fs::read_to_string(output_dir.join("Bytes.lean")).expect("read Bytes.lean");
    assert!(
        bytes.contains("instance : Nonempty Bytes := ⟨⟨[], by simp [Bytes.allInRange]⟩⟩"),
        "Bytes needs a proposition-only empty witness:\n{bytes}"
    );
    assert!(
        !bytes.contains("Inhabited Bytes"),
        "Bytes must never acquire a computable default:\n{bytes}"
    );

    let capability = std::fs::read_to_string(output_dir.join("Cap.lean")).expect("read Cap.lean");
    // `Bytes` is declared in module `Bytes`, so outside that module it is
    // spelled by its owner like every other foreign type (`Bytes.Bytes`).
    assert!(
        capability.contains("\nnoncomputable opaque hash : Bytes.Bytes → Bytes.Bytes\n"),
        "the provider operation must be an explicitly noncomputable opaque:\n{capability}"
    );
    assert!(
        !capability.contains("Inhabited") && !capability.contains("Nonempty"),
        "Bytes.lean already carries the Nonempty instance; the operation must add no witness and no default:\n{capability}"
    );

    let main = std::fs::read_to_string(output_dir.join("Main.lean")).expect("read Main.lean");
    assert!(
        main.contains(
            "noncomputable section\n\n/-- Validate, hash, and unwrap bytes. -/\ndef hashed"
        ) && main.contains("\nend\n\ndef twice (n : Int) : Int :=\n  (n + n)"),
        "only the provider call cone should be noncomputable:\n{main}"
    );
    assert!(
        !main.contains("example : hashed [] = []"),
        "the fabricated-default falsifier must never be decided:\n{main}"
    );

    // These exact declarations are the origin/main emission for the unrelated
    // pure function.  Keeping literal pins makes a provider-dependent sibling
    // unable to perturb either the computable case or the universal law.
    assert!(
        main.contains("def twice (n : Int) : Int :=\n  (n + n)")
            && main.contains("example : twice 2 = 4 := by decide +kernel")
            && main.contains(
                "theorem twice_law_doubling : ∀ (n : Int), twice n = (n * 2) := by\n  intro n\n  simp only [twice] <;> omega"
            )
            && main.contains(
                "theorem twice_law_doubling_checked_domain : (twice 0 = 0) ∧ (twice 1 = 2) ∧ (twice 2 = 4) := by native_decide"
            ),
        "unrelated cases and laws must retain their exact computable emission:\n{main}"
    );
    assert_eq!(summary["universal"].as_bool(), Some(true));
    assert_eq!(summary["universal_laws"].as_u64(), Some(1));

    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
}

#[test]
fn pure_capability_nonempty_witnesses_cover_every_supported_result_family() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability result-family proof test: `lake` not available");
        return;
    }

    let source_dir = temp_output_dir("aver-capability-result-families-src");
    let output_dir = temp_output_dir("aver-capability-result-families-out");
    std::fs::create_dir_all(&source_dir).expect("create capability source dir");
    std::fs::write(
        source_dir.join("shapes.av"),
        r#"module Shapes
    intent = "Every provider codomain family."
    kind = capability
    semantics = pure
    exposes [boolValue, intValue, stringValue, optionValue, resultValue, recordValue, recordAgain, optionRecord, resultRecord, sumValue, handleValue, Wrapped, Choice, Handle]
    effects []

resource Handle

record Wrapped
    value: Int

type Choice
    Empty

operation boolValue() -> Bool
operation intValue() -> Int
operation stringValue() -> String
operation optionValue() -> Option<Int>
operation resultValue() -> Result<Int, String>
operation recordValue() -> Wrapped
operation recordAgain() -> Wrapped
operation optionRecord() -> Option<Wrapped>
operation resultRecord() -> Result<Wrapped, Wrapped>
operation sumValue() -> Choice
operation handleValue() -> Handle
"#,
    )
    .expect("write result-family capability");
    std::fs::write(
        source_dir.join("main.av"),
        r#"module Main
    intent = "Exercise every provider codomain family."
    depends [Shapes]
    exposes [boolCall, intCall, stringCall, optionCall, resultCall, recordCall, sumCall, handleCall]
    effects []

fn boolCall() -> Bool
    Shapes.boolValue()

fn intCall() -> Int
    Shapes.intValue()

fn stringCall() -> String
    Shapes.stringValue()

fn optionCall() -> Option<Int>
    Shapes.optionValue()

fn resultCall() -> Result<Int, String>
    Shapes.resultValue()

fn recordCall() -> Shapes.Wrapped
    Shapes.recordValue()

fn sumCall() -> Shapes.Choice
    Shapes.sumValue()

fn handleCall() -> Shapes.Handle
    Shapes.handleValue()

verify boolCall
    boolCall() => false

verify intCall
    intCall() => 0

verify stringCall
    stringCall() => ""

verify optionCall
    optionCall() => Option.None

verify resultCall
    resultCall() => Result.Err("")

verify recordCall
    recordCall() => Shapes.Wrapped(value = 0)

verify sumCall
    sumCall() => Shapes.Choice.Empty

verify handleCall
    handleCall() => Shapes.handleValue()
"#,
    )
    .expect("write result-family consumer");

    let proof = run_checked_proof(&source_dir, &output_dir);
    let summary = summary_from(&proof);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(8), Some(0)),
        "all provider families must build and every ground claim must decline:\n{}",
        format_output(&proof)
    );

    let shapes = std::fs::read_to_string(output_dir.join("Shapes.lean")).expect("read Shapes.lean");
    // Every family is an explicitly noncomputable opaque, whatever global
    // instance its codomain has.  Lean core already proves `Nonempty` for
    // scalars, containers and anything `Except`/`Option` can reach through
    // one of them, so only records, sums, handles and compounds built
    // solely from those get a proposition-only witness.
    for (name, result_type, witness) in [
        ("boolValue", "Bool", None),
        ("intValue", "Int", None),
        ("stringValue", "String", None),
        ("optionValue", "Option Int", None),
        ("resultValue", "Except String Int", None),
        ("optionRecord", "Option Wrapped", None),
        (
            "recordValue",
            "Wrapped",
            Some("Nonempty Wrapped := ⟨{ value := 0 }⟩"),
        ),
        (
            "recordAgain",
            "Wrapped",
            Some("Nonempty Wrapped := ⟨{ value := 0 }⟩"),
        ),
        (
            "resultRecord",
            "Except Wrapped Wrapped",
            Some("Nonempty (Except Wrapped Wrapped) := ⟨Except.error ({ value := 0 })⟩"),
        ),
        (
            "sumValue",
            "Choice",
            Some("Nonempty Choice := ⟨Choice.empty⟩"),
        ),
        (
            "handleValue",
            "Handle",
            Some("Nonempty Handle := ⟨{ id := 0 }⟩"),
        ),
    ] {
        let declaration = format!("noncomputable opaque {name} : {result_type}\n");
        let expected = match witness {
            Some(witness) => format!("\nlocal instance : {witness}\n{declaration}"),
            None => format!("\n{declaration}"),
        };
        assert!(
            shapes.contains(&expected),
            "unexpected emission for {name}: wanted {expected:?} in\n{shapes}"
        );
    }
    assert_eq!(
        shapes.matches("local instance").count(),
        5,
        "core codomains must not carry redundant witnesses:\n{shapes}"
    );
    // Records and sums keep the `deriving Inhabited` every exported type gets;
    // the explicit `noncomputable` is what stops that default from becoming
    // the operation's value.  The witnesses themselves never add one, and a
    // handle stays without a default, as the runtime never produces one.
    assert!(
        !shapes.contains("instance : Inhabited")
            && shapes.contains(
                "structure Handle where\n  id : Nat\n  deriving Repr, BEq, DecidableEq\n"
            ),
        "capability witnesses must never introduce an Inhabited instance, including for handles:\n{shapes}"
    );

    // The falsifier, straight in Lean: a claim about an operation with a
    // globally `Inhabited` codomain must not compile, because compiling is
    // exactly how `native_decide` would turn `default` into a proof.
    std::fs::write(
        output_dir.join("Probe.lean"),
        "import Shapes\nexample : Shapes.intValue = 0 := by native_decide\n",
    )
    .expect("write Lean probe");
    let probe = Command::new("lake")
        .current_dir(&output_dir)
        .args(["env", "lean", "Probe.lean"])
        .output()
        .expect("run Lean probe");
    let probe_text = format_output(&probe);
    assert!(
        !probe.status.success()
            && probe_text.contains("depends on 'Shapes.intValue', which is 'noncomputable'"),
        "native_decide must be unable to compile a provider operation:\n{probe_text}"
    );

    let declined = summary["declined_claims"]
        .as_array()
        .expect("declined_claims array");
    assert!(declined.iter().all(|claim| {
        claim["kind"] == "cases"
            && claim["reason"]
                .as_str()
                .is_some_and(|reason| reason.contains("opaque and noncomputable"))
    }));

    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
}

#[test]
fn laws_over_a_provider_cone_are_declined_as_a_whole() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability law proof test: `lake` not available");
        return;
    }

    let source_dir = temp_output_dir("aver-capability-opaque-laws-src");
    let output_dir = temp_output_dir("aver-capability-opaque-laws-out");
    std::fs::create_dir_all(&source_dir).expect("create capability source dir");
    std::fs::write(
        source_dir.join("cap.av"),
        r#"module Cap
    intent = "Provider-owned pure operations."
    kind = capability
    semantics = pure
    exposes [hash, hashInt, isValid]
    depends [Bytes]
    effects []

operation hash(input: Bytes) -> Bytes
    ? "Hash bytes through the provider."

operation hashInt(x: Int) -> Int
    ? "Hash an int through the provider."

operation isValid(x: Int) -> Bool
    ? "Validate an int through the provider."
"#,
    )
    .expect("write capability module");
    std::fs::write(
        source_dir.join("main.av"),
        r#"module Main
    intent = "Laws whose samples would be decided by a fabricated provider default."
    depends [Bytes, Cap]
    exposes [h, g, hb, twice]
    effects []

fn h(x: Int) -> Int
    Cap.hashInt(x)

fn g(x: Int) -> Int
    x

fn hb(values: List<Int>) -> List<Int>
    match Bytes.fromList(values)
        Result.Err(_) -> []
        Result.Ok(raw) -> Bytes.toList(Cap.hash(raw))

fn twice(n: Int) -> Int
    n + n

verify h law zero
    given n: Int = 0..2
    h(n) => 0

verify g law guarded
    given n: Int = 0..2
    when Cap.isValid(n)
    g(n) => 7

verify hb law same
    given values: List<Int> = [[], [1]]
    hb(values) => hb(values)

verify twice law doubling
    given n: Int = 0..2
    twice(n) => n * 2
"#,
    )
    .expect("write consumer module");

    let proof = run_checked_proof(&source_dir, &output_dir);
    let summary = summary_from(&proof);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["sorries"].as_u64(),
            summary["universal_laws"].as_u64(),
        ),
        (Some(0), Some(3), Some(0), Some(1)),
        "every provider law is declined whole, nothing is red, and the pure law still proves:\n{}",
        format_output(&proof)
    );
    let declined = summary["declined_claims"]
        .as_array()
        .expect("declined_claims array");
    let refusals: Vec<(&str, &str, &str)> = declined
        .iter()
        .map(|claim| {
            (
                claim["kind"].as_str().unwrap_or_default(),
                claim["claim"].as_str().unwrap_or_default(),
                claim["reason"].as_str().unwrap_or_default(),
            )
        })
        .collect();
    for (claim, operation) in [
        // A false law whose samples equal the fabricated default `0`.
        ("h.zero", "Cap.hashInt"),
        // A guard on the provider: vacuous once `isValid` evaluates to `false`.
        ("g.guarded", "Cap.isValid"),
        // A true law over a Bytes cone: native_decide on a noncomputable
        // function is a build error, so the law must not be stated at all.
        ("hb.same", "Cap.hash"),
    ] {
        assert!(
            refusals.iter().any(|(kind, name, reason)| *kind == "law"
                && *name == claim
                && reason.contains(operation)
                && reason.contains("opaque and noncomputable")),
            "{claim} must be declined naming {operation}: {refusals:?}"
        );
    }

    let main = std::fs::read_to_string(output_dir.join("Main.lean")).expect("read Main.lean");
    for theorem_base in ["h_law_zero", "g_law_guarded", "hb_law_same"] {
        assert!(
            !main.contains(&format!("theorem {theorem_base}")),
            "no universal theorem, checked domain, or sample may be stated for {theorem_base}:\n{main}"
        );
    }
    assert!(
        main.contains("-- verify h: the Lean call cone reaches provider-owned capability operation(s) Cap.hashInt")
            && main.contains("-- verify g: the Lean call cone reaches provider-owned capability operation(s) Cap.isValid")
            && main.contains("-- verify hb: the Lean call cone reaches provider-owned capability operation(s) Cap.hash,"),
        "the exported file must carry each refusal:\n{main}"
    );
    // The pure law in the same module keeps its exact origin/main emission.
    assert!(
        main.contains("\ndef g (x : Int) : Int :=\n  x\n")
            && !main.contains("noncomputable section\n\ndef g")
            && main.contains("def twice (n : Int) : Int :=\n  (n + n)")
            && main.contains(
                "theorem twice_law_doubling : ∀ (n : Int), twice n = (n * 2) := by\n  intro n\n  simp only [twice] <;> omega"
            )
            && main.contains(
                "theorem twice_law_doubling_checked_domain : (twice 0 = 0) ∧ (twice 1 = 2) ∧ (twice 2 = 4) := by native_decide"
            )
            && main.contains("theorem twice_law_doubling_sample_3 : twice 2 = 4 := by native_decide"),
        "functions and laws outside the cone stay computable and provable:\n{main}"
    );

    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
}

#[test]
fn provider_cones_are_followed_through_modules_products_and_given_values() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability cone proof test: `lake` not available");
        return;
    }

    let source_dir = temp_output_dir("aver-capability-opaque-cones-src");
    let output_dir = temp_output_dir("aver-capability-opaque-cones-out");
    std::fs::create_dir_all(&source_dir).expect("create capability source dir");
    std::fs::write(
        source_dir.join("cap.av"),
        r#"module Cap
    intent = "A provider that also defines a pure function over its own operation."
    kind = capability
    semantics = pure
    exposes [hashInt, twiceHash]
    effects []

operation hashInt(x: Int) -> Int
    ? "Hash an int through the provider."

fn twiceHash(x: Int) -> Int
    Cap.hashInt(Cap.hashInt(x))
"#,
    )
    .expect("write capability module");
    std::fs::write(
        source_dir.join("mid.av"),
        r#"module Mid
    intent = "A wrapper one module away from the provider."
    depends [Cap]
    exposes [wrap]
    effects []

fn wrap(x: Int) -> Int
    Cap.hashInt(x) + 0
"#,
    )
    .expect("write wrapper module");
    std::fs::write(
        source_dir.join("main.av"),
        r#"module Main
    intent = "Cones two modules away, through independent products, provider functions, and given values."
    depends [Cap, Mid]
    exposes [w2, both, t, same]
    effects []

fn w2(x: Int) -> Int
    Mid.wrap(x)

fn both(x: Int) -> Int
    match (Cap.hashInt(x), Cap.hashInt(x))!
        (a, b) -> a + b

fn t(x: Int) -> Int
    Cap.twiceHash(x)

fn same(x: Int) -> Int
    x

verify w2
    w2(5) => 0

verify both
    both(3) => 0

verify t
    t(5) => 0

verify same law fromGiven
    given n: Int = [Cap.hashInt(1), 2]
    same(n) => n

verify same
    same(4) => 4
"#,
    )
    .expect("write consumer module");

    let proof = run_checked_proof(&source_dir, &output_dir);
    let summary = summary_from(&proof);
    assert_eq!(
        (
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(0), Some(4), Some(0)),
        "every claim that evaluates the provider is declined and the project still builds:\n{}",
        format_output(&proof)
    );
    let declined = summary["declined_claims"]
        .as_array()
        .expect("declined_claims array");
    let mut identities: Vec<String> = declined
        .iter()
        .map(|claim| {
            format!(
                "{}:{}",
                claim["kind"].as_str().unwrap_or_default(),
                claim["claim"].as_str().unwrap_or_default()
            )
        })
        .collect();
    identities.sort();
    assert_eq!(
        identities,
        ["cases:both", "cases:t", "cases:w2", "law:same.fromGiven"],
        "{declined:?}"
    );
    assert!(
        declined.iter().all(|claim| claim["reason"]
            .as_str()
            .is_some_and(|reason| reason.contains("Cap.hashInt"))),
        "every refusal names the provider operation at the root of the cone: {declined:?}"
    );

    let capability = std::fs::read_to_string(output_dir.join("Cap.lean")).expect("read Cap.lean");
    assert!(
        capability.contains("noncomputable opaque hashInt : Int → Int")
            && capability.contains("noncomputable section\n\ndef twiceHash (x : Int) : Int :=\n  Cap.hashInt (Cap.hashInt x)\n\nend"),
        "a function inside the provider module joins the noncomputable cone:\n{capability}"
    );
    let mid = std::fs::read_to_string(output_dir.join("Mid.lean")).expect("read Mid.lean");
    assert!(
        mid.contains("noncomputable section\n\ndef wrap"),
        "the wrapper is in the cone:\n{mid}"
    );
    let main = std::fs::read_to_string(output_dir.join("Main.lean")).expect("read Main.lean");
    assert!(
        main.contains("noncomputable section\n\ndef w2")
            && main.contains("noncomputable section\n\ndef both")
            && main.contains("noncomputable section\n\ndef t")
            && main.contains("\nend\n\ndef same (x : Int) : Int :=\n  x\n")
            && main.contains("example : same 4 = 4 := by decide +kernel"),
        "only the cone is noncomputable; the pure sibling keeps its computable case:\n{main}"
    );

    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
}
