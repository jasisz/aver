import Bridge.Config

/-!
# Single-instruction bridge lemmas (everything but `call` and `ifElse`)

Each lemma: the wall executes one instruction on a related state, so Talos
takes ONE `Step` to a related state. Talos rules used (SmallStep.lean at the
pinned commit): `localGet` 4876, `localSet` 4882, `const` 4864, `constI64`
4870, `gcFallthrough` 6081 (with `execGcOp`, Semantics.lean:195 — `structGet`
273, `structNew` 257), `refIsNullTrue`/`refIsNullFalse` 4286/4293, `eqI64`
5277, `ltSI64` 5295, `leSI64` 5319, `geSI64` 5331, `gtSI64` 5307, `eq` 5205,
`ltS` 5247, `gtS` 5253, `geS` 5265.

The state is always a Talos thread `⟨params, locs, values⟩` over the store
`⟨synthRuntime m hostEnv, wasm⟩`; only `structNew` changes `wasm`.
-/

namespace Bridge
open Wasm Wasm.SmallStep CertPrelude

/-! ## Machine-word facts -/

theorem constI64_repr {n : Int} (hn : i64Band n) : n = (constI64 n).toInt64.toInt := by
  unfold constI64
  rw [Int64.toInt64_toUInt64, Int64.toInt_ofInt_of_le]
  · exact hn.1
  · exact hn.2

theorem constI32_repr {n : Int} (hn : i32Band n) : n = (constI32 n).toInt32.toInt := by
  unfold constI32
  rw [Int32.toInt32_toUInt32, Int32.toInt_ofInt_of_le]
  · exact hn.1
  · exact hn.2

theorem UInt64.toInt64_toInt_inj (x y : UInt64) : x.toInt64.toInt = y.toInt64.toInt ↔ x = y := by
  rw [Int64.toInt_inj]
  constructor
  · intro h
    have := congrArg Int64.toUInt64 h
    simpa using this
  · rintro rfl
    rfl

theorem UInt32.toInt32_toInt_inj (x y : UInt32) : x.toInt32.toInt = y.toInt32.toInt ↔ x = y := by
  rw [Int32.toInt_inj]
  constructor
  · intro h
    have := congrArg Int32.toUInt32 h
    simpa using this
  · rintro rfl
    rfl

/-! ## Locals -/

theorem bridge_localGet {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (code : Program) (arity : Nat) (remainder : List Value)
    (controls : List ControlFrame) (calls : List CallFrame)
    (locA : List WVal) (i : Nat) (v : WVal)
    (hL : RLocals wasm.gcHeap ⟨params, locs, values⟩ locA)
    (hA : locA[i]? = some v) :
    ∃ tv, Step ⟨.running ⟨⟨params, locs, values⟩, .localGet i :: code, arity, remainder, controls, calls⟩,
              ⟨rt, wasm⟩⟩
          (.instruction (.localGet i))
          ⟨.running ⟨⟨params, locs, tv :: values⟩, code, arity, remainder, controls, calls⟩, ⟨rt, wasm⟩⟩
        ∧ R wasm.gcHeap tv v := by
  obtain ⟨tv, htv, hR⟩ := Rs_getElem? i v hL hA
  refine ⟨tv, Step.localGet ?_, hR⟩
  rw [Locals.get_eq]
  exact htv

theorem bridge_localSet {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (tv : Value) (code : Program) (arity : Nat)
    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame)
    (locA : List WVal) (i : Nat) (v : WVal)
    (hi : i < locA.length)
    (hL : RLocals wasm.gcHeap ⟨params, locs, values⟩ locA) (hv : R wasm.gcHeap tv v) :
    ∃ L' : Locals,
      Step ⟨.running ⟨⟨params, locs, tv :: values⟩, .localSet i :: code, arity, remainder, controls, calls⟩,
              ⟨rt, wasm⟩⟩
          (.instruction (.localSet i))
          ⟨.running ⟨{ L' with values := values }, code, arity, remainder, controls, calls⟩, ⟨rt, wasm⟩⟩
        ∧ RLocals wasm.gcHeap { L' with values := values } (locA.set i v) := by
  have hlen : (params ++ locs).length = locA.length := Rs_length hL
  obtain ⟨L', hset, hflat, -⟩ :=
    Locals.set?_eq ⟨params, locs, tv :: values⟩ i tv (by simpa [hlen] using hi)
  refine ⟨L', Step.localSet hset, ?_⟩
  unfold RLocals
  simp only
  rw [hflat]
  exact Rs_set i hL hv

/-! ## Constants -/

theorem bridge_i64Const {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (code : Program) (arity : Nat) (remainder : List Value)
    (controls : List ControlFrame) (calls : List CallFrame) (n : Int) (hn : i64Band n) :
    Step ⟨.running ⟨⟨params, locs, values⟩, .constI64 (constI64 n) :: code, arity, remainder, controls, calls⟩,
            ⟨rt, wasm⟩⟩
        (.instruction (.constI64 (constI64 n)))
        ⟨.running ⟨⟨params, locs, .i64 (constI64 n) :: values⟩, code, arity, remainder, controls, calls⟩,
            ⟨rt, wasm⟩⟩
      ∧ R wasm.gcHeap (.i64 (constI64 n)) (.i64v n) :=
  ⟨Step.constI64, constI64_repr hn⟩

theorem bridge_i32Const {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (code : Program) (arity : Nat) (remainder : List Value)
    (controls : List ControlFrame) (calls : List CallFrame) (n : Int) (hn : i32Band n) :
    Step ⟨.running ⟨⟨params, locs, values⟩, .const (constI32 n) :: code, arity, remainder, controls, calls⟩,
            ⟨rt, wasm⟩⟩
        (.instruction (.const (constI32 n)))
        ⟨.running ⟨⟨params, locs, .i32 (constI32 n) :: values⟩, code, arity, remainder, controls, calls⟩,
            ⟨rt, wasm⟩⟩
      ∧ R wasm.gcHeap (.i32 (constI32 n)) (.i32v n) :=
  ⟨Step.const, constI32_repr hn⟩

/-! ## Structs -/

theorem bridge_structGet {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (tv : Value) (code : Program) (arity : Nat)
    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame)
    (ty t f : Nat) (fs : List WVal) (v : WVal)
    (hv : R wasm.gcHeap tv (.structv t fs)) (hf : fs[f]? = some v) :
    ∃ tv', Step ⟨.running ⟨⟨params, locs, tv :: values⟩, .gc (.structGet ty f) :: code, arity, remainder,
              controls, calls⟩, ⟨rt, wasm⟩⟩
          (.instruction (.gc (.structGet ty f)))
          ⟨.running ⟨⟨params, locs, tv' :: values⟩, code, arity, remainder, controls, calls⟩, ⟨rt, wasm⟩⟩
        ∧ R wasm.gcHeap tv' v := by
  obtain ⟨a, vs, rfl, hget, hvs⟩ := R_structv hv
  obtain ⟨tv', htv', hR⟩ := Rs_getElem? f v hvs hf
  refine ⟨tv', Step.gcFallthrough ?_, hR⟩
  simp only [execGcOp, hget, htv']

theorem zip_pack_id :
    ∀ (fields : List FieldType) (args : List Value),
      fields.length = args.length → (∀ ft ∈ fields, ∀ v, ft.pack v = v) →
      (fields.zip args).map (fun (ft, v) => ft.pack v) = args
  | [], [], _, _ => rfl
  | ft :: fields, v :: args, hlen, hpack => by
      simp only [List.zip_cons_cons, List.map_cons]
      rw [hpack ft (by simp) v, zip_pack_id fields args (by simpa using hlen)
        (fun ft' h => hpack ft' (by simp [h]))]
  | [], _ :: _, hlen, _ => by simp at hlen
  | _ :: _, [], hlen, _ => by simp at hlen

theorem bridge_structNew {α : Type} (env : TranslateEnv) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (fbody : Program) (hostEnv : HostEnv α) (wasm : Store α)
    (params locs values : List Value) (code : Program) (arity : Nat)
    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame)
    (ty : Nat) (sorts : List STy) (hs : structSorts? env.structs ty = some sorts)
    (stA : List WVal) (hS : Rs wasm.gcHeap values stA) (hlen : ¬ stA.length < sorts.length) :
    ∃ wasm',
      Step ⟨.running ⟨⟨params, locs, values⟩, .gc (.structNew ty) :: code, arity, remainder, controls, calls⟩,
              ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm⟩⟩
          (.instruction (.gc (.structNew ty)))
          ⟨.running ⟨⟨params, locs,
              .anyref (some (.struct wasm.gcHeap.length)) :: values.drop sorts.length⟩,
              code, arity, remainder, controls, calls⟩,
              ⟨synthRuntime (synthModule env paramSorts result nlocals fbody) hostEnv, wasm'⟩⟩
        ∧ wasm.gcHeap <+: wasm'.gcHeap
        ∧ Rs wasm'.gcHeap (.anyref (some (.struct wasm.gcHeap.length)) :: values.drop sorts.length)
            (.structv ty (stA.take sorts.length).reverse :: stA.drop sorts.length) := by
  have hfields := synthModule_structFields hs paramSorts result nlocals fbody
  have hlenEq := Rs_length hS
  have hargsLen : (sorts.map fieldTypeOf).length = ((values.take sorts.length).reverse).length := by
    simp only [List.length_map, List.length_reverse, List.length_take]
    omega
  refine ⟨{ wasm with gcHeap := wasm.gcHeap ++ [.struct ty (values.take sorts.length).reverse] },
    ?_, ⟨_, rfl⟩, ?_⟩
  · refine Step.gcFallthrough ?_
    simp only [synthRuntime_currentModule, execGcOp, hfields, List.length_map]
    rw [if_neg (by omega)]
    rw [zip_pack_id (sorts.map fieldTypeOf) _ hargsLen
      (fun ft hft v => by
        obtain ⟨t, -, rfl⟩ := List.mem_map.mp hft
        exact fieldTypeOf_pack t v)]
  · simp only [Rs, R]
    refine ⟨⟨(values.take sorts.length).reverse, by simp, ?_⟩, ?_⟩
    · exact Rs_append _ _ _ _ (Rs_reverse (Rs_take sorts.length hS))
    · exact Rs_append _ _ _ _ (Rs_drop sorts.length hS)

/-! ## `ref.is_null` -/

theorem bridge_refIsNull {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (tv : Value) (code : Program) (arity : Nat)
    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame)
    (w : WVal) (hw : R wasm.gcHeap tv w) (b : Bool)
    (hb : (w = .null → b = true) ∧ ((∃ t fs, w = .structv t fs) → b = false) ∧
      ((∃ t es, w = .arr t es) → b = false)) (hsort : w = .null ∨ (∃ t fs, w = .structv t fs) ∨ (∃ t es, w = .arr t es)) :
    ∃ r, Step ⟨.running ⟨⟨params, locs, tv :: values⟩, .refIsNull :: code, arity, remainder, controls, calls⟩,
              ⟨rt, wasm⟩⟩
          (.instruction .refIsNull)
          ⟨.running ⟨⟨params, locs, .i32 r :: values⟩, code, arity, remainder, controls, calls⟩, ⟨rt, wasm⟩⟩
        ∧ R wasm.gcHeap (.i32 r) (b32 b) := by
  rcases hsort with rfl | ⟨t, fs, rfl⟩ | ⟨t, es, rfl⟩
  · rw [R_null hw]
    rw [hb.1 rfl]
    exact ⟨1, Step.refIsNullTrue rfl, by simp [b32, R]⟩
  · obtain ⟨a, vs, rfl, -, -⟩ := R_structv hw
    rw [hb.2.1 ⟨t, fs, rfl⟩]
    exact ⟨0, Step.refIsNullFalse rfl, by simp [b32, R]⟩
  · obtain ⟨a, vs, rfl, -, -⟩ := R_arr hw
    rw [hb.2.2 ⟨t, es, rfl⟩]
    exact ⟨0, Step.refIsNullFalse rfl, by simp [b32, R]⟩

/-! ## Comparisons

The wall pushes `b32 (decide p)`, Talos `.i32 (if q then 1 else 0)`; `R_b32`
needs `p ↔ q`, which is the signed-order transfer through `toInt`. -/

theorem bridge_i64Cmp {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (lhs rhs : UInt64) (code : Program) (arity : Nat)
    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame)
    (a b : Int) (ha : a = lhs.toInt64.toInt) (hb : b = rhs.toInt64.toInt)
    (env : TranslateEnv) (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (op : WInstr) (hop : op ∈ i64Cmps) (instr : Instruction) (htr : translate env op = some instr) :
    ∃ r w,
      (∀ (rest : List WInstr) (locA stA : List WVal),
        wRunF host ar callee (op :: rest) locA (.i64v b :: .i64v a :: stA) =
          wRunF host ar callee rest locA (w :: stA)) ∧
      HasSort env w .i32 ∧
      Step ⟨.running ⟨⟨params, locs, .i64 rhs :: .i64 lhs :: values⟩, instr :: code, arity, remainder,
              controls, calls⟩, ⟨rt, wasm⟩⟩
          (.instruction instr)
          ⟨.running ⟨⟨params, locs, .i32 r :: values⟩, code, arity, remainder, controls, calls⟩, ⟨rt, wasm⟩⟩
        ∧ R wasm.gcHeap (.i32 r) w := by
  subst ha hb
  simp only [i64Cmps, List.mem_cons, List.not_mem_nil, or_false] at hop
  rcases hop with rfl | rfl | rfl | rfl | rfl <;>
    simp only [translate, Option.some.injEq] at htr <;> subst htr
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.eqI64 rfl,
      R_b32 _ _ (UInt64.toInt64_toInt_inj lhs rhs)⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.ltSI64 rfl,
      R_b32 _ _ Int64.lt_iff_toInt_lt.symm⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.leSI64 rfl,
      R_b32 _ _ Int64.le_iff_toInt_le.symm⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.geSI64 rfl,
      R_b32 _ _ Int64.le_iff_toInt_le.symm⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.gtSI64 rfl,
      R_b32 _ _ Int64.lt_iff_toInt_lt.symm⟩

theorem bridge_i32Cmp {α : Type} (rt : RuntimeEnv α) (wasm : Store α)
    (params locs values : List Value) (lhs rhs : UInt32) (code : Program) (arity : Nat)
    (remainder : List Value) (controls : List ControlFrame) (calls : List CallFrame)
    (a b : Int) (ha : a = lhs.toInt32.toInt) (hb : b = rhs.toInt32.toInt)
    (env : TranslateEnv) (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (op : WInstr) (hop : op ∈ i32Cmps) (instr : Instruction) (htr : translate env op = some instr) :
    ∃ r w,
      (∀ (rest : List WInstr) (locA stA : List WVal),
        wRunF host ar callee (op :: rest) locA (.i32v b :: .i32v a :: stA) =
          wRunF host ar callee rest locA (w :: stA)) ∧
      HasSort env w .i32 ∧
      Step ⟨.running ⟨⟨params, locs, .i32 rhs :: .i32 lhs :: values⟩, instr :: code, arity, remainder,
              controls, calls⟩, ⟨rt, wasm⟩⟩
          (.instruction instr)
          ⟨.running ⟨⟨params, locs, .i32 r :: values⟩, code, arity, remainder, controls, calls⟩, ⟨rt, wasm⟩⟩
        ∧ R wasm.gcHeap (.i32 r) w := by
  subst ha hb
  simp only [i32Cmps, List.mem_cons, List.not_mem_nil, or_false] at hop
  rcases hop with rfl | rfl | rfl | rfl <;>
    simp only [translate, Option.some.injEq] at htr <;> subst htr
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.eq rfl,
      R_b32 _ _ (UInt32.toInt32_toInt_inj lhs rhs)⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.ltS rfl,
      R_b32 _ _ Int32.lt_iff_toInt_lt.symm⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.gtS rfl,
      R_b32 _ _ Int32.lt_iff_toInt_lt.symm⟩
  · exact ⟨_, _, fun _ _ _ => by simp [wRunF], HasSort_b32 _, Step.geS rfl,
      R_b32 _ _ Int32.le_iff_toInt_le.symm⟩

end Bridge
