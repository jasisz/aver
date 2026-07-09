fn render_string_eq_verbatim_match_cert(c: &Cert) -> String {
    let Cert::StringEqVerbatimMatch {
        name,
        self_idx,
        arms,
        default,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let [(needle, hit)] = arms.as_slice() else {
        unreachable!()
    };
    let VerbatimDefault::Array {
        type_idx: string_ty,
        data_idx: _,
        bytes: _,
    } = needle
    else {
        unreachable!()
    };
    let needle_w = render_wval(needle);
    let needle_arg = render_wval_arg(needle);
    let hit_guard = render_default_guard(hit);
    let miss_input = VerbatimDefault::Array {
        type_idx: *string_ty,
        data_idx: 0,
        bytes: vec![120],
    };
    let miss_guard = match default {
        StringEqDefault::Input => render_default_guard(&miss_input),
        StringEqDefault::Verbatim(k) => render_default_guard(k),
    };
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, b32, popArgs, initLocals, List.set"
    );
    format!(
        r#"/-! ### {name} — String.eq host-contract verbatim match certificate -/

/-- The VERBATIM emitted body compares the input with one byte-derived String
    literal via the contracted `String.eq` host slot, then returns either the
    byte-derived hit literal or the byte-exact default. The loop inside the
    helper is outside this user-code proof and enters only through `hStringEq`. -/
theorem {name}_wasm_certified
    (stringEq : List WVal → Option WVal)
    (hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b)) :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN {name}Code ({name}Host stringEq) (fuel + 1) {self_idx} [v] = some w →
      w = {name}Model v := by
  intro fuel v w hrun
  cases v with
  | i32v n => simp [{evalset}] at hrun
  | i64v n => simp [{evalset}] at hrun
  | f64v bits => simp [{evalset}] at hrun
  | null => simp [{evalset}] at hrun
  | structv t fs =>
      by_cases ht : t = {string_ty}
      · subst ht
        cases hcall : stringEq [.structv {string_ty} fs, {needle_w}] with
        | none =>
            simp [{evalset}, hcall] at hrun
        | some got =>
            have hgot : got = b32 (stringEqW (.structv {string_ty} fs) {needle_arg}) :=
              hStringEq (.structv {string_ty} fs) {needle_arg} got hcall
            have hmatch : stringEqW (.structv {string_ty} fs) {needle_arg} = false := by rfl
            simp [{evalset}, hcall, hgot, hmatch] at hrun
            simpa [{name}Model, hmatch] using hrun.symm
      · simp [{evalset}, ht] at hrun
  | arr t es =>
      by_cases ht : t = {string_ty}
      · subst ht
        cases hcall : stringEq [.arr {string_ty} es, {needle_w}] with
        | none =>
            simp [{evalset}, hcall] at hrun
        | some got =>
            have hgot : got = b32 (stringEqW (.arr {string_ty} es) {needle_arg}) :=
              hStringEq (.arr {string_ty} es) {needle_arg} got hcall
            cases hmatch : stringEqW (.arr {string_ty} es) {needle_arg}
            · simp [{evalset}, hcall, hgot, hmatch] at hrun
              simpa [{name}Model, hmatch] using hrun.symm
            · simp [{evalset}, hcall, hgot, hmatch] at hrun
              simpa [{name}Model, hmatch] using hrun.symm
      · simp [{evalset}, ht] at hrun

#print axioms {name}_wasm_certified

-- Executable tripwires: one equal input takes the hit arm; a distinct byte
-- string takes the default arm. The host reference is the executable face of
-- the same String.eq contract used abstractly above.
def {name}HostRef : HostTbl := {name}Host stringEqRef
example :
    (wFuncN {name}Code {name}HostRef 8 {self_idx} [{needle_w}]).bind {hit_guard} := by
  native_decide
example :
    (wFuncN {name}Code {name}HostRef 8 {self_idx} [.arr {string_ty} [.i32v 120]]).bind {miss_guard} := by
  native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel v vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      have hc := {name}_wasm_certified stringEq hStringEq f v w hrun
      simpa [AverCert.Schema.verbatimRepr] using hc
"#,
    )
}

/// Render the certificate for a `String.concat` verbatim match: a loop-free
/// body that builds a container of string arrays (one or more `array.new_data`
/// literals plus the input), invokes the contracted `String.concat` host slot
/// once, and returns the byte-concatenated result. The proof reduces the
/// straight-line body to the host call, then discharges via `hStringConcat`.
fn render_string_concat_verbatim_match_cert(c: &Cert) -> String {
    let Cert::StringConcatVerbatimMatch {
        name,
        self_idx,
        string_concat_idx: _,
        container_ty,
        result_ty,
        prefixes,
        suffixes,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    // Render the literal WVal prefixes/suffixes for the model. The model
    // prepends each prefix byte-list and appends each suffix byte-list to the
    // input's bytes, all under the container's type index (the type the
    // backend assigns to the concat result, distinct from the input's).
    let prefix_parts: Vec<String> = prefixes.iter().map(render_wval_qualified).collect();
    let suffix_parts: Vec<String> = suffixes.iter().map(render_wval_qualified).collect();
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, stringConcatW, wByteAppend, b32, popArgs, initLocals, List.set"
    );
    // The container built by the body, in Lean form, so the proof can cite the
    // contract on exactly this value.
    let mut container_parts = String::new();
    for p in &prefix_parts {
        container_parts.push_str(&format!("{p}, "));
    }
    container_parts.push('v');
    for s in &suffix_parts {
        container_parts.push_str(&format!(", {s}"));
    }
    format!(
        r#"/-! ### {name} — String.concat host-contract verbatim match certificate -/

/-- The VERBATIM emitted body builds a container of byte-derived String
    literals plus the input, invokes the contracted `String.concat` host slot
    once, and returns the byte-concatenated result. The loop inside the helper
    is outside this user-code proof and enters only through `hStringConcat`. -/
theorem {name}_wasm_certified
    (stringConcat : Nat → List WVal → Option WVal)
    (hStringConcat : ∀ resultTy parts c, stringConcat resultTy [parts] = some c → c = stringConcatW resultTy parts) :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN {name}Code ({name}Host stringConcat) (fuel + 1) {self_idx} [v] = some w →
      w = {name}Model v := by
  intro fuel v w hrun
  cases hcall : stringConcat {result_ty} [WVal.arr {container_ty} [{container_parts}]] with
  | none => simp [{evalset}, hcall] at hrun
  | some got =>
      have hgot : got = stringConcatW {result_ty} (WVal.arr {container_ty} [{container_parts}]) :=
        hStringConcat {result_ty} (WVal.arr {container_ty} [{container_parts}]) got hcall
      simp [{evalset}, hcall, hgot] at hrun ⊢
      exact hrun.symm

#print axioms {name}_wasm_certified

-- Executable tripwire: the concat of the witness input produces the expected
-- byte array. The host reference is the executable face of the same
-- String.concat contract used abstractly above.
def {name}HostRef : HostTbl := {name}Host stringConcatRef

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel v vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      have hc := {name}_wasm_certified stringConcat hStringConcat f v w hrun
      simpa [AverCert.Schema.verbatimRepr] using hc
"#,
    )
}
