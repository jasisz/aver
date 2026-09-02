import Bridge.EnvOfClaim

set_option autoImplicit false

/-!
# Coverage: every lowered compute-face body is typed and translatable

Brief §3/§9 (1). `lowerExprFragmentBody carrier plan = some instrs` is the
wall's own lowering of a plan that passed `PlanCheck.checkExprFragmentRawPlan`
(the check is the guard inside `lowerExprFragmentBody`, so it is not a
separate hypothesis here). This file proves, by induction over
`PlanLower.lowerNodesFuel`/`lowerBlockFuel` (verbatim in `AverMin.lean`),
that such a body has a stack typing `HasTy env (Γof plan.params) [] instrs
[sortOfFragTy plan.result]` and hence translates (`translateList env instrs =
some _`, `HasTy_translates`).

* The lowerer's SYMBOLIC STACK (a list of node ids) is the typing context:
  `σ = stack.map (sortAt nodes)`, where `sortAt` reads the node's declared
  `FragTy` and `sortOfFragTy : FragTy → STy` is the sort map.
* The checker's facts are read one node at a time: `checkBlockFuel (cf+1)` is
  DEFINITIONALLY `checkNodes (inferNodeKindTy cf params) [] block.nodes && …`
  (`checkBlockFuel_succ` is `rfl`; `inferNodeKindTy` is the checker's local
  `let`, named so the proof can unfold it).
* Nested `ifElse` is the block lemma one fuel level down (the lowerer and the
  checker both descend by one on a branch), and the `intSignCmp` template
  (`localSet` of the declared scratch local `params.length`) is typed once
  (`HasTy_intSignCmpTemplate`) against the fixed carrier layout.

## Profile membership (`nodeInProfile`)

What the CHECKER does not decide and the bridge needs, stated as a Bool
predicate with EXPLICIT arms over every `FragNodeKind` constructor (a new
constructor makes the match non-exhaustive: the build stops):

* out of profile (brief §3): `constF64Bits`, `selfCall` (recursion families),
  `vectorGetOrDefault` (String/Vector family), the `f64*` and `i32And` prims;
* in profile but with a side condition the checker leaves to the byte pin:
  `constI64`/`constI32` literals must be in the machine band (the checker
  types them at any `Int`; the record-compute classifier does pin
  `constI64`, `recordComputeNodeOk`), a `hostCall` must resolve through the
  environment's import table to its role's signature and `box` must box a
  literal (the only argument shape under which `boxRef`'s result is
  canonical, `CarrierSpec.canonSmall`), a `structGetUser`/
  `structNew` must cite a DECLARED struct index whose declared field sorts
  agree with the node's types (this is the envelope gap of brief §9: the
  layout comes from a `typedecl-v1` claim, not from the compute plan);
* in profile unconditionally: everything else (the checker's typing is
  enough).

`Tripwire.lean` is the executable enumeration over both grammars.
-/

namespace Bridge
open Wasm CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.PlanLower
open AverCert.Schema (CarrierSpec)

/-! ## The sort map -/

/-- `FragTy → STy`: the coarse wasm sort of each representation type. An
    `intCarrier` is a canonical carrier (`.car`): that is what the checker's
    typing means for a value the host contracts consume, and what the
    record-compute face's domain representation supplies. -/
def sortOfFragTy : FragTy → STy
  | .f64 => .f64
  | .boolI32 => .i32
  | .intCarrier => .car
  | .i64 => .i64
  | .rawI32 => .i32
  | .ref => .ref
  | .adtRef => .ref

/-- The sort of one node: its declared type's sort, except that an `i64`
    LITERAL node is an `.i64b` (the operand shape `box` needs). -/
def sortOfNode (n : FragNode) : STy :=
  match n.kind with
  | .constI64 _ =>
      match n.ty with
      | .i64 => .i64b
      | ty => sortOfFragTy ty
  | _ => sortOfFragTy n.ty

/-- The sort of a node id of a block (the default is never consulted on a
    checked plan: every id on the symbolic stack names a checked node). -/
def sortAt (nodes : List FragNode) (id : Nat) : STy :=
  match lookupNode nodes id with
  | some n => sortOfNode n
  | none => .i32

/-- Locals of a compute-face body: the parameters, then the ONE declared
    carrier scratch local (`AcceptedArtifact.exprFragmentNLocals = 1`). -/
def Γof (params : List FragTy) : List STy := params.map sortOfFragTy ++ [.ref]

theorem sortOfNode_sub (n : FragNode) : SubSort (sortOfNode n) (sortOfFragTy n.ty) := by
  unfold sortOfNode
  cases hk : n.kind <;> try exact SubSort.refl _
  cases hty : n.ty <;> first | exact SubSort.refl _ | exact Or.inr (Or.inl ⟨rfl, rfl⟩)

theorem sortOfNode_of_ne (n : FragNode) (h : n.ty ≠ .i64) : sortOfNode n = sortOfFragTy n.ty := by
  unfold sortOfNode
  cases hk : n.kind <;> try rfl
  cases hty : n.ty <;> first | rfl | exact absurd hty h

theorem sortOfNode_isI64 (n : FragNode) (h : n.ty = .i64) : IsI64 (sortOfNode n) := by
  unfold sortOfNode
  rw [h]
  cases hk : n.kind <;> first | exact Or.inr rfl | exact Or.inl rfl

theorem sortOfNode_constI64 (n : FragNode) (v : Int) (hk : n.kind = .constI64 v) (h : n.ty = .i64) :
    sortOfNode n = .i64b := by
  unfold sortOfNode
  rw [hk, h]

theorem lookupNode_prefix {checked : List FragNode} (more : List FragNode) {a : Nat} {n : FragNode}
    (h : lookupNode checked a = some n) : lookupNode (checked ++ more) a = some n := by
  simp only [lookupNode] at h ⊢
  rw [List.getElem?_append_left (List.getElem?_eq_some_iff.mp h).1]
  exact h

theorem lookupTy_of_lookupNode {nodes : List FragNode} {id : Nat} {n : FragNode}
    (h : lookupNode nodes id = some n) : lookupTy nodes id = some n.ty := by
  simp [lookupTy, h]

theorem lookupNode_of_lookupTy {nodes : List FragNode} {id : Nat} {t : FragTy}
    (h : lookupTy nodes id = some t) : ∃ n, lookupNode nodes id = some n ∧ n.ty = t := by
  simp only [lookupTy] at h
  split at h
  · rename_i n hn
    simp only [Option.some.injEq] at h
    exact ⟨n, hn, h⟩
  · simp at h

theorem sortAt_of_lookupNode {nodes : List FragNode} {a : Nat} {n : FragNode}
    (h : lookupNode nodes a = some n) : sortAt nodes a = sortOfNode n := by
  simp [sortAt, h]

theorem sameTy_eq {a b : FragTy} (h : sameTy a b = true) : a = b := by
  simpa [sameTy] using h

theorem hasTy_lookupTy {checked : List FragNode} {a : Nat} {t : FragTy}
    (h : hasTy checked a t = true) : lookupTy checked a = some t := by
  simp only [hasTy] at h
  split at h
  · rename_i got hg
    rw [sameTy_eq h] at hg
    exact hg
  · simp at h

/-- The sort of a checked argument of a non-`i64` type, read off the whole
    block (`checked` is a prefix). -/
theorem sortAt_of_hasTy_ne {checked : List FragNode} (more : List FragNode) {a : Nat} {t : FragTy}
    (h : hasTy checked a t = true) (hne : t ≠ .i64) :
    sortAt (checked ++ more) a = sortOfFragTy t := by
  obtain ⟨n, hn, rfl⟩ := lookupNode_of_lookupTy (hasTy_lookupTy h)
  rw [sortAt_of_lookupNode (lookupNode_prefix more hn), sortOfNode_of_ne n hne]

theorem sortAt_of_hasTy_i64 {checked : List FragNode} (more : List FragNode) {a : Nat}
    (h : hasTy checked a .i64 = true) : IsI64 (sortAt (checked ++ more) a) := by
  obtain ⟨n, hn, hty⟩ := lookupNode_of_lookupTy (hasTy_lookupTy h)
  rw [sortAt_of_lookupNode (lookupNode_prefix more hn)]
  exact sortOfNode_isI64 n hty

theorem hasI32Ty_sort {checked : List FragNode} (more : List FragNode) {a : Nat}
    (h : hasI32Ty checked a = true) : sortAt (checked ++ more) a = .i32 := by
  simp only [hasI32Ty] at h
  split at h
  · rename_i hl
    obtain ⟨n, hn, hty⟩ := lookupNode_of_lookupTy hl
    rw [sortAt_of_lookupNode (lookupNode_prefix more hn), sortOfNode_of_ne n (by rw [hty]; decide),
      hty]
    rfl
  · rename_i hl
    obtain ⟨n, hn, hty⟩ := lookupNode_of_lookupTy hl
    rw [sortAt_of_lookupNode (lookupNode_prefix more hn), sortOfNode_of_ne n (by rw [hty]; decide),
      hty]
    rfl
  · simp at h

theorem lookupNode_self {checked : List FragNode} {node : FragNode} {rest : List FragNode}
    (hid : node.id = checked.length) : lookupNode (checked ++ node :: rest) node.id = some node := by
  simp only [lookupNode]
  rw [hid, List.getElem?_append_right (Nat.le_refl _), Nat.sub_self]
  rfl

/-- Arguments of non-`i64` types have exactly their types' sorts. -/
theorem argsHaveTys_sorts {checked : List FragNode} (more : List FragNode) :
    ∀ {args : List Nat} {tys : List FragTy}, argsHaveTys checked args tys = true →
      (∀ t ∈ tys, t ≠ .i64) → args.map (sortAt (checked ++ more)) = tys.map sortOfFragTy
  | [], [], _, _ => rfl
  | a :: args, t :: tys, h, hne => by
      simp only [argsHaveTys, Bool.and_eq_true] at h
      simp only [List.map_cons, argsHaveTys_sorts more h.2 (fun t ht => hne t (List.mem_cons_of_mem _ ht)),
        sortAt_of_hasTy_ne more h.1 (hne t List.mem_cons_self)]
  | [], _ :: _, h, _ => by simp [argsHaveTys] at h
  | _ :: _, [], h, _ => by simp [argsHaveTys] at h

theorem popExpected_eq {stack : List Nat} {v : Nat} {s' : List Nat}
    (h : popExpected stack v = some s') : stack = v :: s' := by
  cases stack with
  | nil => simp [popExpected] at h
  | cons got r =>
    simp only [popExpected] at h
    split at h
    · rename_i heq
      subst heq
      simp only [Option.some.injEq] at h
      subst h
      rfl
    · simp at h

theorem popExpectedAll_eq : ∀ {stack l s' : List Nat},
    popExpectedAll stack l = some s' → stack = l ++ s'
  | stack, [], s', h => by
      simp only [popExpectedAll, Option.some.injEq] at h
      subst h
      rfl
  | stack, v :: l, s', h => by
      simp only [popExpectedAll] at h
      split at h
      · rename_i s₁ hp
        rw [popExpected_eq hp, popExpectedAll_eq h]
        rfl
      · simp at h

theorem Γof_param {params : List FragTy} {index : Nat} {ty : FragTy}
    (h : params[index]? = some ty) : (Γof params)[index]? = some (sortOfFragTy ty) := by
  unfold Γof
  rw [List.getElem?_append_left (by simpa using (List.getElem?_eq_some_iff.mp h).1),
    List.getElem?_map, h]
  rfl

theorem Γof_scratch (params : List FragTy) : (Γof params)[params.length]? = some .ref := by
  unfold Γof
  rw [List.getElem?_append_right (by simp), List.length_map, Nat.sub_self]
  rfl

theorem inI64Band_band {k : Int} (h : inI64Band k = true) : i64Band k := by
  simp only [inI64Band, Bool.and_eq_true, decide_eq_true_eq] at h
  exact (i64Band_iff k).mpr h

theorem i32Band_bool (v : Bool) : i32Band (if v then 1 else 0) := by
  cases v <;> simp [i32Band]

theorem carrierFieldTy?_sort : ∀ {field : Nat} {ty : FragTy}, carrierFieldTy? field = some ty →
    [STy.i64, .ref, .i32][field]? = some (sortOfFragTy ty)
  | 0, _, h => by simp only [carrierFieldTy?, Option.some.injEq] at h; subst h; rfl
  | 1, _, h => by simp only [carrierFieldTy?, Option.some.injEq] at h; subst h; rfl
  | 2, _, h => by simp only [carrierFieldTy?, Option.some.injEq] at h; subst h; rfl
  | _ + 3, _, h => by simp [carrierFieldTy?] at h

/-! ## The checker, one node at a time

`inferNodeKindTy fuel params` is the local `let` of `checkBlockFuel (fuel + 1)
params` (PlanCheck.lean lines 246–330), given a name; `checkBlockFuel_succ`
is `rfl` and is the only place the copy is trusted — if the wall's checker
changes, the `rfl` breaks. -/

def inferNodeKindTy (fuel : Nat) (params : List FragTy) (checked : List FragNode) (node : FragNode) :
    Option FragTy :=
  match node.kind with
  | .local index => params[index]?
  | .constBool _ => some .boolI32
  | .constI64 _ => some .i64
  | .constI32 _ => some .rawI32
  | .constF64Bits _ => some .f64
  | .structGet field receiver =>
      if hasTy checked receiver .intCarrier then carrierFieldTy? field else none
  -- v1 admits three field reads out of a user struct: the opaque
  -- reference-field projection (`adtRef`, flowed verbatim through the
  -- field-projection face), the scalar `i32` tag/discriminant read
  -- (`rawI32`, e.g. the Option/Result tag) that a tag-dispatch feeds into
  -- `i32`-typed primitives, and the record-declaration scalar field read
  -- (`intCarrier`/`boolI32`/`f64` via `fragTyIsRecordScalar`), whose
  -- declared type is confirmed against the module's type section by the
  -- record face's equality pin over the certified Plan declaration. The
  -- plan DECLARES which via `node.ty`; the byte-exact gate and decoded
  -- struct context bind `tyIdx`/`field` and confirm the field's real
  -- storage. A wrong declaration lowers to bytes whose read yields the
  -- wrong `WVal` kind and traps (fail-closed). The scalar admission
  -- cannot leak into the generic path: `genericFragmentAllowedFuel`
  -- rejects EVERY `structGetUser` node outright, so a scalar-typed read
  -- is only acceptable through the record-parameter classify branch,
  -- whose exact two-node shape and byte pins gate it.
  | .structGetUser _tyIdx _field value =>
      if hasTy checked value .adtRef &&
          (node.ty = .adtRef || node.ty = .rawI32 ||
            fragTyIsRecordScalar node.ty)
      then some node.ty else none
  -- Construction of a user struct from already-computed values: yields
  -- the opaque reference. Field-count/type agreement with the module's
  -- type section is byte-side work (the type index is bound by the
  -- byte-exact gate); the structural check demands the args exist as
  -- typed nodes. Like `structGetUser`, the generic path never admits it
  -- (`genericFragmentAllowedFuel` rejects it outright).
  | .structNew _tyIdx args =>
      if !args.isEmpty &&
          args.all (fun a => (lookupNode checked a).isSome) then
        some .adtRef
      else none
  | .refIsNull value =>
      if hasTy checked value .ref && isCarrierLimbField checked value
      then some .boolI32
      else none
  | .prim op args => primResultTy? checked op args
  | .hostCall role _funcIdx args => hostCallResultTy? checked role args
  -- A self-call yields the Int carrier when every argument is an Int
  -- carrier. `funcIdx` is not typed here; artifact acceptance binds it to
  -- the byte-derived self index,
  -- mirroring `hostCall`.
  | .selfCall _tail _funcIdx args =>
      if !args.isEmpty && fragArgsAllTy checked .intCarrier args then
        some .intCarrier
      else none
  | .ifElse cond thenBlock elseBlock =>
      if hasTy checked cond .boolI32 &&
         checkBlockFuel fuel params thenBlock &&
         checkBlockFuel fuel params elseBlock then
        match lookupNode thenBlock.nodes thenBlock.result,
              lookupNode elseBlock.nodes elseBlock.result with
        | some t, some e => if t.ty = e.ty then some t.ty else none
        | _, _ => none
      else none
  -- The monolithic fused vector read hard-references locals 0 (vector)
  -- and 1 (index), so it types only under exactly that param prefix.
  | .vectorGetOrDefault _arrTy _toIndexIdx _boxIdx _default =>
      if params[0]? = some .adtRef && params[1]? = some .intCarrier then
        some .intCarrier
      else none
  -- The sign template consumes ONE Int carrier and yields the source
  -- Boolean. Two pins live here rather than in a face: the scratch slot
  -- is exactly the one declared local (`params.length`), so the template
  -- cannot write over a parameter, and the literal must be inside the
  -- i64 band, which is what makes its limb-carrying arm exact.
  | .intSignCmp _op constant scratch value =>
      if hasTy checked value .intCarrier && scratch = params.length &&
          inI64Band constant then
        some .boolI32
      else none

theorem checkBlockFuel_succ (fuel : Nat) (params : List FragTy) (block : FragBlock) :
    checkBlockFuel (fuel + 1) params block =
      (checkBlockFuel.checkNodes (inferNodeKindTy fuel params) [] block.nodes &&
        match lookupNode block.nodes block.result with
        | some n => n.id = block.result && block.result + 1 = block.nodes.length
        | none => false) := by
  rfl

theorem checkNodes_cons (f : List FragNode → FragNode → Option FragTy) (checked : List FragNode)
    (node : FragNode) (rest : List FragNode)
    (h : checkBlockFuel.checkNodes f checked (node :: rest) = true) :
    node.id = checked.length ∧ (∃ ty, f checked node = some ty ∧ node.ty = ty) ∧
      checkBlockFuel.checkNodes f (checked ++ [node]) rest = true := by
  simp only [checkBlockFuel.checkNodes, Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨hid, hty⟩, hrest⟩ := h
  refine ⟨hid, ?_, hrest⟩
  split at hty
  · rename_i ty hf
    exact ⟨ty, hf, sameTy_eq hty⟩
  · simp at hty

/-! ## Profile membership -/

/-- The primitives of the profile. Every `FragPrim` constructor is listed. -/
def primInProfile : FragPrim → Bool
  | .f64Add => false
  | .f64Mul => false
  | .f64Le => false
  | .f64Ge => false
  | .f64Lt => false
  | .f64Gt => false
  | .f64Eq => false
  | .i64Eq => true
  | .i64LeS => true
  | .i64LtS => true
  | .i64GeS => true
  | .i64GtS => true
  | .i32Eq => true
  | .i32LtS => true
  | .i32GtS => true
  | .i32GeS => true
  | .i32And => false

/-- Profile membership of one node of a block (`nodes` = the block's nodes,
    for the sorts of `structNew` arguments), relative to the environment.
    Every `FragNodeKind` constructor is listed; `fuel` descends on `ifElse`
    exactly like the checker's. Fuel exhaustion answers `false`. -/
def nodeInProfile : Nat → TranslateEnv → List FragNode → FragNode → Bool
  | 0, _, _, _ => false
  | fuel + 1, env, nodes, node =>
      match node.kind with
      | .local _ => true
      | .constBool _ => true
      | .constI64 value => decide (i64Band value)
      | .constI32 value => decide (i32Band value)
      | .constF64Bits _ => false
      | .structGet _ _ => true
      | .structGetUser tyIdx field _ =>
          match structSorts? env.structs tyIdx with
          | some fs => fs[field]? == some (sortOfFragTy node.ty)
          | none => false
      | .refIsNull _ => true
      | .prim op _ => primInProfile op
      | .hostCall role funcIdx args =>
          (match slotLookup? env.imports funcIdx with
            | some (_, sig) => sig.params == (roleSig role).1 && sig.result == (roleSig role).2
            | none => false) &&
          -- `box` boxes a LITERAL (the emitter's `i64.const k; call box` idiom):
          -- that is the one argument shape under which its result is canonical.
          (match role, args with
            | .box, [a] =>
                match lookupNode nodes a with
                | some n => match n.kind with | .constI64 _ => true | _ => false
                | none => false
            | .box, _ => false
            | _, _ => true)
      | .selfCall _ _ _ => false
      | .ifElse _ thenBlock elseBlock =>
          thenBlock.nodes.all (nodeInProfile fuel env thenBlock.nodes) &&
            elseBlock.nodes.all (nodeInProfile fuel env elseBlock.nodes)
      | .vectorGetOrDefault _ _ _ _ => false
      | .structNew tyIdx args =>
          structSorts? env.structs tyIdx == some (args.map (sortAt nodes))
      | .intSignCmp _ _ _ _ => true

def blockInProfile (fuel : Nat) (env : TranslateEnv) (block : FragBlock) : Bool :=
  block.nodes.all (nodeInProfile fuel env block.nodes)

/-- Profile membership of a plan, at the wall's canonical fuel. -/
def planInProfile (env : TranslateEnv) (plan : ExprFragmentRawPlan) : Bool :=
  blockInProfile AverCert.PlanCheck.maxFuel env plan.body

/-! ## Typing facts -/

theorem HasTy_append {env : TranslateEnv} {Γ : List STy} :
    ∀ {σ σ₁ : List STy} {is₁ : List WInstr}, HasTy env Γ σ is₁ σ₁ →
      ∀ {σ₂ : List STy} {is₂ : List WInstr}, HasTy env Γ σ₁ is₂ σ₂ →
        HasTy env Γ σ (is₁ ++ is₂) σ₂ := by
  intro σ σ₁ is₁ h₁
  induction h₁ with
  | nil => intro σ₂ is₂ h₂; simpa using h₂
  | localGet h _ ih => intro σ₂ is₂ h₂; exact .localGet h (ih h₂)
  | localSet h hsub _ ih => intro σ₂ is₂ h₂; exact .localSet h hsub (ih h₂)
  | i64Const hn _ ih => intro σ₂ is₂ h₂; exact .i64Const hn (ih h₂)
  | i32Const hn _ ih => intro σ₂ is₂ h₂; exact .i32Const hn (ih h₂)
  | structGet h₀ hs hf _ ih => intro σ₂ is₂ h₂; exact .structGet h₀ hs hf (ih h₂)
  | structNew hs _ ih => intro σ₂ is₂ h₂; exact .structNew hs (ih h₂)
  | refIsNull _ ih => intro σ₂ is₂ h₂; exact .refIsNull (ih h₂)
  | call hs _ ih => intro σ₂ is₂ h₂; exact .call hs (ih h₂)
  | i64Cmp h₁ h₂ hop _ ih => intro σ₂ is₂ hc; exact .i64Cmp h₁ h₂ hop (ih hc)
  | i32Cmp hop _ ih => intro σ₂ is₂ h₂; exact .i32Cmp hop (ih h₂)
  | ifElse ht he h₁ h₂ _ _ _ ih => intro σ₂ is₂ hc; exact .ifElse ht he h₁ h₂ (ih hc)

theorem translateList_cons_of {env : TranslateEnv} {x : WInstr} {x' : Instruction}
    {xs : List WInstr} {c : Program} (hx : translate env x = some x')
    (hxs : translateList env xs = some c) : translateList env (x :: xs) = some (x' :: c) := by
  simp [translateList, hx, hxs]

theorem translate_i64Cmp {env : TranslateEnv} {op : WInstr} (hop : op ∈ i64Cmps) :
    ∃ x, translate env op = some x := by
  simp only [i64Cmps, List.mem_cons, List.not_mem_nil, or_false] at hop
  rcases hop with rfl | rfl | rfl | rfl | rfl
  · exact ⟨.eqI64, by simp [translate]⟩
  · exact ⟨.ltSI64, by simp [translate]⟩
  · exact ⟨.leSI64, by simp [translate]⟩
  · exact ⟨.geSI64, by simp [translate]⟩
  · exact ⟨.gtSI64, by simp [translate]⟩

theorem translate_i32Cmp {env : TranslateEnv} {op : WInstr} (hop : op ∈ i32Cmps) :
    ∃ x, translate env op = some x := by
  simp only [i32Cmps, List.mem_cons, List.not_mem_nil, or_false] at hop
  rcases hop with rfl | rfl | rfl | rfl
  · exact ⟨.eq, by simp [translate]⟩
  · exact ⟨.ltS, by simp [translate]⟩
  · exact ⟨.gtS, by simp [translate]⟩
  · exact ⟨.geS, by simp [translate]⟩

/-- Every typed instruction list translates: the side conditions of the
    typing rules are exactly the side conditions of `translate`. -/
theorem HasTy_translates {env : TranslateEnv} {Γ : List STy} :
    ∀ {σ : List STy} {is : List WInstr} {σ' : List STy}, HasTy env Γ σ is σ' →
      ∃ code, translateList env is = some code := by
  intro σ is σ' h
  induction h with
  | nil => exact ⟨[], rfl⟩
  | @localGet _ _ i _ _ _ _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .localGet i) (by simp [translate]) hc⟩
  | @localSet _ _ i _ _ _ _ _ _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .localSet i) (by simp [translate]) hc⟩
  | @i64Const _ _ n _ hn _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .constI64 (constI64 n)) (by simp [translate, hn]) hc⟩
  | @i32Const _ _ n _ hn _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .const (constI32 n)) (by simp [translate, hn]) hc⟩
  | @structGet _ _ ty f _ _ _ _ _ _ _ _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .gc (.structGet ty f)) (by simp [translate]) hc⟩
  | @structNew _ _ ty _ _ hs _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .gc (.structNew ty)) (by simp [translate, hs]) hc⟩
  | refIsNull _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .refIsNull) (by simp [translate]) hc⟩
  | @call _ _ _ i _ _ hs _ ih =>
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .call i) (by simp [translate, hs]) hc⟩
  | i64Cmp _ _ hop _ ih =>
      obtain ⟨c, hc⟩ := ih
      obtain ⟨x, hx⟩ := translate_i64Cmp (env := env) hop
      exact ⟨_, translateList_cons_of hx hc⟩
  | i32Cmp hop _ ih =>
      obtain ⟨c, hc⟩ := ih
      obtain ⟨x, hx⟩ := translate_i32Cmp (env := env) hop
      exact ⟨_, translateList_cons_of hx hc⟩
  | ifElse _ _ _ _ _ iht ihe ih =>
      obtain ⟨ct, hct⟩ := iht
      obtain ⟨ce, hce⟩ := ihe
      obtain ⟨c, hc⟩ := ih
      exact ⟨_, translateList_cons_of (x' := .iff 0 1 ct ce [] [.anyref])
        (by simp [translate, hct, hce]) hc⟩

/-- The sign template is typed `carrier → i32` against the carrier layout,
    with the declared scratch local `params.length` (a `.ref` slot: the
    template stores the operand — a canonical carrier, `.car` — into it,
    `SubSort .car .ref`, and reads it back as a declared struct). -/
theorem HasTy_intSignCmpTemplate (env : TranslateEnv) (carrier : Nat)
    (hcar : structSorts? env.structs carrier = some [.i64, .ref, .i32])
    (params : List FragTy) (op : SymIntCmp) (k : Int) (hk : i64Band k) (t₀ : STy)
    (h₀ : SubSort t₀ .ref) (σ : List STy) :
    HasTy env (Γof params) (t₀ :: σ) (intSignCmpTemplate carrier params.length op k) (.i32 :: σ) := by
  have hscr := Γof_scratch params
  have hsmall : HasTy env (Γof params) []
      [.localGet params.length, .structGet carrier 0, .i64Const k,
        primInstr (intSignCmpSmallPrim op)] [.i32] := by
    refine .localGet hscr (.structGet (Or.inl rfl) hcar rfl
      (.i64Const hk (.i64Cmp (Or.inr rfl) (Or.inl rfl) ?_ .nil)))
    cases op <;> simp [intSignCmpSmallPrim, primInstr, i64Cmps]
  have hbig : HasTy env (Γof params) [] (intSignCmpBigArm carrier params.length op) [.i32] := by
    cases op <;> simp only [intSignCmpBigArm]
    · exact .i32Const (by simp [i32Band]) .nil
    · exact .localGet hscr (.structGet (Or.inl rfl) hcar rfl (.i32Const (by simp [i32Band])
        (.i32Cmp (by simp [i32Cmps]) .nil)))
    · exact .localGet hscr (.structGet (Or.inl rfl) hcar rfl (.i32Const (by simp [i32Band])
        (.i32Cmp (by simp [i32Cmps]) .nil)))
    · exact .localGet hscr (.structGet (Or.inl rfl) hcar rfl (.i32Const (by simp [i32Band])
        (.i32Cmp (by simp [i32Cmps]) .nil)))
    · exact .localGet hscr (.structGet (Or.inl rfl) hcar rfl (.i32Const (by simp [i32Band])
        (.i32Cmp (by simp [i32Cmps]) .nil)))
  exact .localSet hscr h₀ (.localGet hscr (.structGet (Or.inl rfl) hcar rfl
    (.refIsNull (.ifElse hsmall hbig (SubSort.refl _) (SubSort.refl _) .nil))))

/-! ### Host-call and primitive shapes fixed by the checker -/

/-- Argument types of a host role as the checker demands them
    (`hostCallResultTy?`); `toIndex` has no admitted standalone call, its
    entry is only what makes `roleSig_sorts` total. -/
def roleArgTys : HostRole → List FragTy
  | .box => [.i64]
  | .add => [.intCarrier, .intCarrier]
  | .mul => [.intCarrier, .intCarrier]
  | .sub => [.intCarrier, .intCarrier]
  | .toIndex => [.intCarrier]
  | .cmp => [.intCarrier, .intCarrier]
  | .eq => [.intCarrier, .intCarrier]

def roleResTy : HostRole → FragTy
  | .box => .intCarrier
  | .add => .intCarrier
  | .mul => .intCarrier
  | .sub => .intCarrier
  | .toIndex => .rawI32
  | .cmp => .rawI32
  | .eq => .boolI32

/-- For every role but `box` the signature is the sort image of the checker's
    argument types; `box` refines its `i64` operand to a literal (`.i64b`). -/
theorem roleSig_sorts (role : HostRole) (hne : role ≠ .box) :
    (roleSig role).1 = (roleArgTys role).map sortOfFragTy ∧
      (roleSig role).2 = sortOfFragTy (roleResTy role) := by
  cases role <;> first | exact absurd rfl hne | exact ⟨rfl, rfl⟩

theorem roleArgTys_ne_i64 (role : HostRole) (hne : role ≠ .box) :
    ∀ t ∈ roleArgTys role, t ≠ .i64 := by
  cases role <;> simp_all [roleArgTys]

theorem hostCallResultTy?_some {checked : List FragNode} {role : HostRole} {args : List Nat}
    {ty : FragTy} (h : hostCallResultTy? checked role args = some ty) :
    argsHaveTys checked args (roleArgTys role) = true ∧ ty = roleResTy role := by
  cases role <;> simp only [hostCallResultTy?] at h <;> (try split at h) <;>
    simp_all [roleArgTys, roleResTy]

theorem primInProfile_cases {op : FragPrim} (h : primInProfile op = true) :
    op ∈ [FragPrim.i64Eq, .i64LeS, .i64LtS, .i64GeS, .i64GtS] ∨
      op ∈ [FragPrim.i32Eq, .i32LtS, .i32GtS, .i32GeS] := by
  cases op <;> simp_all [primInProfile]

theorem prim_i64 {checked : List FragNode} {op : FragPrim} {args : List Nat} {ty : FragTy}
    (hop : op ∈ [FragPrim.i64Eq, .i64LeS, .i64LtS, .i64GeS, .i64GtS])
    (h : primResultTy? checked op args = some ty) :
    ∃ a b, args = [a, b] ∧ hasTy checked a .i64 = true ∧ hasTy checked b .i64 = true ∧
      ty = .boolI32 ∧ primInstr op ∈ i64Cmps := by
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hop
  rcases hop with rfl | rfl | rfl | rfl | rfl
  all_goals
    simp only [primResultTy?] at h
    split at h
    · rename_i hargs
      simp only [Option.some.injEq] at h
      match args, hargs with
      | [a, b], hargs =>
        simp only [argsHaveTys, Bool.and_eq_true, and_true] at hargs
        exact ⟨a, b, rfl, hargs.1, hargs.2, h.symm, by simp [primInstr, i64Cmps]⟩
      | [], hargs => simp [argsHaveTys] at hargs
      | [_], hargs => simp [argsHaveTys] at hargs
      | _ :: _ :: _ :: _, hargs => simp [argsHaveTys] at hargs
    · simp at h

theorem prim_i32 {checked : List FragNode} {op : FragPrim} {args : List Nat} {ty : FragTy}
    (hop : op ∈ [FragPrim.i32Eq, .i32LtS, .i32GtS, .i32GeS])
    (h : primResultTy? checked op args = some ty) :
    ∃ a b, args = [a, b] ∧ hasI32Ty checked a = true ∧ hasI32Ty checked b = true ∧
      ty = .boolI32 ∧ primInstr op ∈ i32Cmps := by
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hop
  rcases hop with rfl | rfl | rfl | rfl
  all_goals
    simp only [primResultTy?] at h
    split at h
    · rename_i a b
      split at h
      · rename_i hab
        simp only [Bool.and_eq_true] at hab
        simp only [Option.some.injEq] at h
        exact ⟨a, b, rfl, hab.1, hab.2, h.symm, by simp [primInstr, i32Cmps]⟩
      · simp at h
    · simp at h

/-! ## The induction -/

def NodesCov (lf : Nat) : Prop :=
  ∀ (env : TranslateEnv) (carrier : Nat),
    structSorts? env.structs carrier = some [.i64, .ref, .i32] →
    ∀ (params : List FragTy) (cf pf : Nat) (nodes checked rest : List FragNode)
      (stack : List Nat) (instrs : List WInstr) (finalStack : List Nat),
      nodes = checked ++ rest →
      checkBlockFuel.checkNodes (inferNodeKindTy cf params) checked rest = true →
      rest.all (nodeInProfile pf env nodes) = true →
      lowerNodesFuel lf carrier rest stack = some (instrs, finalStack) →
      HasTy env (Γof params) (stack.map (sortAt nodes)) instrs (finalStack.map (sortAt nodes))

def BlockCov (lf : Nat) : Prop :=
  ∀ (env : TranslateEnv) (carrier : Nat),
    structSorts? env.structs carrier = some [.i64, .ref, .i32] →
    ∀ (params : List FragTy) (cf pf : Nat) (block : FragBlock) (instrs : List WInstr),
      checkBlockFuel cf params block = true →
      blockInProfile pf env block = true →
      lowerBlockFuel lf carrier block = some instrs →
      HasTy env (Γof params) [] instrs [sortAt block.nodes block.result]

theorem covStep : ∀ lf, (∀ m, m < lf → NodesCov m ∧ BlockCov m) → NodesCov lf ∧ BlockCov lf := by
  intro lf ihStrong
  cases lf with
  | zero =>
    constructor
    · intro env carrier _ params cf pf nodes checked rest stack instrs fs _ _ _ hlow
      simp [lowerNodesFuel] at hlow
    · intro env carrier _ params cf pf block instrs _ _ hlow
      simp [lowerBlockFuel] at hlow
  | succ lf =>
    have ih := ihStrong lf (Nat.lt_succ_self lf)
    constructor
    · intro env carrier hcar params cf pf nodes checked rest stack instrs fs hn hchk hprof hlow
      cases rest with
      | nil =>
        simp only [lowerNodesFuel, Option.some.injEq, Prod.mk.injEq] at hlow
        obtain ⟨rfl, rfl⟩ := hlow
        exact .nil
      | cons node rest =>
        have hn' : nodes = (checked ++ [node]) ++ rest := by simpa using hn
        obtain ⟨hid, ⟨ty, hinf, hty⟩, hchk'⟩ := checkNodes_cons _ _ _ _ hchk
        subst hty
        simp only [List.all_cons, Bool.and_eq_true] at hprof
        obtain ⟨hprof1, hprof2⟩ := hprof
        have hself : sortAt nodes node.id = sortOfNode node :=
          sortAt_of_lookupNode (hn ▸ lookupNode_self hid)
        have hpre : ∀ {a : Nat} {t : FragTy}, hasTy checked a t = true → t ≠ .i64 →
            sortAt nodes a = sortOfFragTy t := fun h hne =>
          hn ▸ sortAt_of_hasTy_ne _ h hne
        -- The rest of the block, one fuel level down (the lowerer's own accounting).
        have restIH : ∀ (stack' : List Nat) (r : List WInstr) (f : List Nat),
            lowerNodesFuel lf carrier rest (node.id :: stack') = some (r, f) →
            HasTy env (Γof params) (sortOfNode node :: stack'.map (sortAt nodes)) r
              (f.map (sortAt nodes)) := by
          intro stack' r f hrest
          have := ih.1 env carrier hcar params cf pf nodes (checked ++ [node]) rest (node.id :: stack')
            r f hn' hchk' hprof2 hrest
          simpa [hself] using this
        cases pf with
        | zero => simp [nodeInProfile] at hprof1
        | succ pf =>
        simp only [lowerNodesFuel] at hlow
        cases hk : node.kind <;> simp only [hk] at hlow
        case «local» index =>
          simp only [inferNodeKindTy, hk] at hinf
          split at hlow
          · rename_i r f hrest
            simp only [Option.some.injEq, Prod.mk.injEq] at hlow
            obtain ⟨rfl, rfl⟩ := hlow
            have hih := restIH _ _ _ hrest
            rw [show sortOfNode node = sortOfFragTy node.ty by unfold sortOfNode; rw [hk]] at hih
            exact HasTy_append (.localGet (Γof_param hinf) .nil) hih
          · simp at hlow
        case constBool value =>
          simp only [inferNodeKindTy, hk, Option.some.injEq] at hinf
          split at hlow
          · rename_i r f hrest
            simp only [Option.some.injEq, Prod.mk.injEq] at hlow
            obtain ⟨rfl, rfl⟩ := hlow
            have hih := restIH _ _ _ hrest
            simp only [sortOfNode, hk, ← hinf, sortOfFragTy] at hih
            exact HasTy_append (.i32Const (i32Band_bool value) .nil) hih
          · simp at hlow
        case constI64 value =>
          simp only [inferNodeKindTy, hk, Option.some.injEq] at hinf
          simp only [nodeInProfile, hk, decide_eq_true_eq] at hprof1
          split at hlow
          · rename_i r f hrest
            simp only [Option.some.injEq, Prod.mk.injEq] at hlow
            obtain ⟨rfl, rfl⟩ := hlow
            have hih := restIH _ _ _ hrest
            rw [sortOfNode_constI64 node value hk hinf.symm] at hih
            exact HasTy_append (.i64Const hprof1 .nil) hih
          · simp at hlow
        case constI32 value =>
          simp only [inferNodeKindTy, hk, Option.some.injEq] at hinf
          simp only [nodeInProfile, hk, decide_eq_true_eq] at hprof1
          split at hlow
          · rename_i r f hrest
            simp only [Option.some.injEq, Prod.mk.injEq] at hlow
            obtain ⟨rfl, rfl⟩ := hlow
            have hih := restIH _ _ _ hrest
            simp only [sortOfNode, hk, ← hinf, sortOfFragTy] at hih
            exact HasTy_append (.i32Const hprof1 .nil) hih
          · simp at hlow
        case constF64Bits bits =>
          simp [nodeInProfile, hk] at hprof1
        case structGet field receiver =>
          simp only [inferNodeKindTy, hk] at hinf
          split at hinf
          · rename_i hrecv
            cases hp : popExpected stack receiver with
            | none => simp [hp] at hlow
            | some stack' =>
              simp only [hp] at hlow
              obtain rfl := popExpected_eq hp
              split at hlow
              · rename_i r f hrest
                simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                have hih := restIH _ _ _ hrest
                rw [show sortOfNode node = sortOfFragTy node.ty by unfold sortOfNode; rw [hk]] at hih
                simp only [List.map_cons, hpre hrecv (by decide), sortOfFragTy]
                exact HasTy_append (.structGet (Or.inr rfl) hcar (carrierFieldTy?_sort hinf) .nil) hih
              · simp at hlow
          · simp at hinf
        case structGetUser tyIdx field value =>
          simp only [inferNodeKindTy, hk] at hinf
          split at hinf
          · rename_i hc
            simp only [Bool.and_eq_true] at hc
            simp only [nodeInProfile, hk] at hprof1
            split at hprof1
            · rename_i fs' hfs
              simp only [beq_iff_eq] at hprof1
              cases hp : popExpected stack value with
              | none => simp [hp] at hlow
              | some stack' =>
                simp only [hp] at hlow
                obtain rfl := popExpected_eq hp
                split at hlow
                · rename_i r f hrest
                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                  obtain ⟨rfl, rfl⟩ := hlow
                  have hih := restIH _ _ _ hrest
                  rw [show sortOfNode node = sortOfFragTy node.ty by unfold sortOfNode; rw [hk]] at hih
                  simp only [List.map_cons, hpre hc.1 (by decide), sortOfFragTy]
                  exact HasTy_append (.structGet (Or.inl rfl) hfs hprof1 .nil) hih
                · simp at hlow
            · simp at hprof1
          · simp at hinf
        case refIsNull value =>
          simp only [inferNodeKindTy, hk] at hinf
          split at hinf
          · rename_i hc
            simp only [Bool.and_eq_true] at hc
            simp only [Option.some.injEq] at hinf
            cases hp : popExpected stack value with
            | none => simp [hp] at hlow
            | some stack' =>
              simp only [hp] at hlow
              obtain rfl := popExpected_eq hp
              split at hlow
              · rename_i r f hrest
                simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                have hih := restIH _ _ _ hrest
                simp only [sortOfNode, hk, ← hinf, sortOfFragTy] at hih
                simp only [List.map_cons, hpre hc.1 (by decide), sortOfFragTy]
                exact HasTy_append (.refIsNull .nil) hih
              · simp at hlow
          · simp at hinf
        case prim op args =>
          simp only [inferNodeKindTy, hk] at hinf
          simp only [nodeInProfile, hk] at hprof1
          cases hp : popExpectedAll stack args.reverse with
          | none => simp [hp] at hlow
          | some stack' =>
            simp only [hp] at hlow
            obtain rfl := popExpectedAll_eq hp
            split at hlow
            · rename_i r f hrest
              simp only [Option.some.injEq, Prod.mk.injEq] at hlow
              obtain ⟨rfl, rfl⟩ := hlow
              have hih := restIH _ _ _ hrest
              simp only [sortOfNode, hk] at hih
              rcases primInProfile_cases hprof1 with h64 | h32
              · obtain ⟨a, b, rfl, ha, hb, hres, hmem⟩ := prim_i64 h64 hinf
                have hσ : List.map (sortAt nodes) ([a, b].reverse ++ stack') =
                    sortAt nodes b :: sortAt nodes a :: List.map (sortAt nodes) stack' := by
                  simp
                rw [hσ]
                rw [hres] at hih
                exact HasTy_append (.i64Cmp (hn ▸ sortAt_of_hasTy_i64 _ hb)
                  (hn ▸ sortAt_of_hasTy_i64 _ ha) hmem .nil) hih
              · obtain ⟨a, b, rfl, ha, hb, hres, hmem⟩ := prim_i32 h32 hinf
                have hσ : List.map (sortAt nodes) ([a, b].reverse ++ stack') =
                    .i32 :: .i32 :: List.map (sortAt nodes) stack' := by
                  simp [hn ▸ hasI32Ty_sort _ ha, hn ▸ hasI32Ty_sort _ hb]
                rw [hσ]
                rw [hres] at hih
                exact HasTy_append (.i32Cmp hmem .nil) hih
            · simp at hlow
        case hostCall role funcIdx args =>
          simp only [inferNodeKindTy, hk] at hinf
          simp only [nodeInProfile, hk, Bool.and_eq_true] at hprof1
          obtain ⟨hslot', hbox⟩ := hprof1
          split at hslot'
          · rename_i i sig hslot
            simp only [Bool.and_eq_true, beq_iff_eq] at hslot'
            obtain ⟨hargs, hres⟩ := hostCallResultTy?_some hinf
            cases hp : popExpectedAll stack args.reverse with
            | none => simp [hp] at hlow
            | some stack' =>
              simp only [hp] at hlow
              obtain rfl := popExpectedAll_eq hp
              split at hlow
              · rename_i r f hrest
                simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                have hih := restIH _ _ _ hrest
                simp only [sortOfNode, hk] at hih
                rw [hres] at hih
                by_cases hb : role = .box
                · subst hb
                  -- `box`: one literal argument.
                  simp only [roleArgTys] at hargs
                  match args, hargs, hbox with
                  | [a], hargs, hbox =>
                    simp only [argsHaveTys, Bool.and_eq_true, and_true] at hargs
                    obtain ⟨n, hna, hnty⟩ := lookupNode_of_lookupTy (hasTy_lookupTy hargs)
                    have hna' : lookupNode nodes a = some n := hn ▸ lookupNode_prefix _ hna
                    simp only [hna'] at hbox
                    split at hbox
                    · rename_i v hkv
                      have hsa : sortAt nodes a = .i64b := by
                        rw [sortAt_of_lookupNode hna', sortOfNode_constI64 n v hkv hnty]
                      have hσ : List.map (sortAt nodes) ([a].reverse ++ stack') =
                          sig.params.reverse ++ List.map (sortAt nodes) stack' := by
                        simp [hsa, hslot'.1, roleSig]
                      rw [hσ]
                      rw [show sortOfFragTy (roleResTy .box) = sig.result by
                        rw [hslot'.2]; rfl] at hih
                      exact HasTy_append (.call hslot .nil) hih
                    · simp at hbox
                · have hsorts := argsHaveTys_sorts (node :: rest) hargs (roleArgTys_ne_i64 role hb)
                  rw [← hn] at hsorts
                  have hσ : List.map (sortAt nodes) (args.reverse ++ stack') =
                      sig.params.reverse ++ List.map (sortAt nodes) stack' := by
                    rw [List.map_append, List.map_reverse, hsorts, ← (roleSig_sorts role hb).1,
                      ← hslot'.1]
                  rw [hσ]
                  rw [← (roleSig_sorts role hb).2, ← hslot'.2] at hih
                  exact HasTy_append (.call hslot .nil) hih
              · simp at hlow
          · simp at hslot'
        case selfCall tail funcIdx args =>
          simp [nodeInProfile, hk] at hprof1
        case ifElse cond thenBlock elseBlock =>
          simp only [inferNodeKindTy, hk] at hinf
          simp only [nodeInProfile, hk, Bool.and_eq_true] at hprof1
          obtain ⟨hpt, hpe⟩ := hprof1
          split at hinf
          · rename_i hc
            simp only [Bool.and_eq_true] at hc
            obtain ⟨⟨hcond, hct⟩, hce⟩ := hc
            split at hinf
            · rename_i tn en htn hen
              split at hinf
              · rename_i hte
                simp only [Option.some.injEq] at hinf
                cases hp : popExpected stack cond with
                | none => simp [hp] at hlow
                | some stack' =>
                  simp only [hp] at hlow
                  obtain rfl := popExpected_eq hp
                  cases ht : lowerBlockFuel lf carrier thenBlock with
                  | none => simp [ht] at hlow
                  | some tI =>
                    cases he : lowerBlockFuel lf carrier elseBlock with
                    | none => simp [he] at hlow
                    | some eI =>
                      simp only [ht, he] at hlow
                      split at hlow
                      · rename_i r f hrest
                        simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                        obtain ⟨rfl, rfl⟩ := hlow
                        have hih := restIH _ _ _ hrest
                        rw [show sortOfNode node = sortOfFragTy node.ty by unfold sortOfNode; rw [hk], ← hinf] at hih
                        have hT := ih.2 env carrier hcar params cf pf thenBlock tI hct hpt ht
                        have hE := ih.2 env carrier hcar params cf pf elseBlock eI hce hpe he
                        rw [sortAt_of_lookupNode htn] at hT
                        rw [sortAt_of_lookupNode hen] at hE
                        simp only [List.map_cons, hpre hcond (by decide), sortOfFragTy]
                        exact HasTy_append (.ifElse hT hE (sortOfNode_sub tn)
                          (hte ▸ sortOfNode_sub en) .nil) hih
                      · simp at hlow
              · simp at hinf
            · simp at hinf
          · simp at hinf
        case vectorGetOrDefault arrTy toIndexIdx boxIdx default =>
          simp [nodeInProfile, hk] at hprof1
        case structNew tyIdx args =>
          simp only [inferNodeKindTy, hk] at hinf
          split at hinf
          · simp only [Option.some.injEq] at hinf
            simp only [nodeInProfile, hk, beq_iff_eq] at hprof1
            cases hp : popExpectedAll stack args.reverse with
            | none => simp [hp] at hlow
            | some stack' =>
              simp only [hp] at hlow
              obtain rfl := popExpectedAll_eq hp
              split at hlow
              · rename_i r f hrest
                simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                have hih := restIH _ _ _ hrest
                simp only [sortOfNode, hk, ← hinf, sortOfFragTy] at hih
                have hσ : List.map (sortAt nodes) (args.reverse ++ stack') =
                    (args.map (sortAt nodes)).reverse ++ List.map (sortAt nodes) stack' := by
                  rw [List.map_append, List.map_reverse]
                rw [hσ]
                have hlen : args.length = (args.map (sortAt nodes)).length := by simp
                rw [hlen]
                exact HasTy_append (.structNew hprof1 .nil) hih
              · simp at hlow
          · simp at hinf
        case intSignCmp op k scratch value =>
          simp only [inferNodeKindTy, hk] at hinf
          split at hinf
          · rename_i hc
            simp only [Bool.and_eq_true, decide_eq_true_eq] at hc
            obtain ⟨⟨hval, hscr⟩, hband⟩ := hc
            simp only [Option.some.injEq] at hinf
            subst hscr
            cases hp : popExpected stack value with
            | none => simp [hp] at hlow
            | some stack' =>
              simp only [hp] at hlow
              obtain rfl := popExpected_eq hp
              split at hlow
              · rename_i r f hrest
                simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                have hih := restIH _ _ _ hrest
                simp only [sortOfNode, hk, ← hinf, sortOfFragTy] at hih
                simp only [List.map_cons, hpre hval (by decide), sortOfFragTy]
                exact HasTy_append
                  (HasTy_intSignCmpTemplate env carrier hcar params op k (inI64Band_band hband) .car
                    (Or.inr (Or.inr ⟨rfl, rfl⟩)) _) hih
              · simp at hlow
          · simp at hinf
    · intro env carrier hcar params cf pf block instrs hchk hprof hlow
      simp only [lowerBlockFuel] at hlow
      cases cf with
      | zero => simp [checkBlockFuel] at hchk
      | succ cf =>
      rw [checkBlockFuel_succ] at hchk
      simp only [Bool.and_eq_true] at hchk
      obtain ⟨hnodes, -⟩ := hchk
      cases hn : lowerNodesFuel lf carrier block.nodes [] with
      | none => simp [hn] at hlow
      | some pair =>
        obtain ⟨is, fs⟩ := pair
        rw [hn] at hlow
        cases fs with
        | nil => simp at hlow
        | cons r rs =>
          cases rs with
          | cons r' rs => simp at hlow
          | nil =>
            by_cases hr : r = block.result
            · subst hr
              have his : is = instrs := by simpa using hlow
              subst his
              have := ih.1 env carrier hcar params cf pf block.nodes [] block.nodes [] is
                [block.result] rfl hnodes hprof hn
              simpa using this
            · simp [hr] at hlow

theorem cov (lf : Nat) : NodesCov lf ∧ BlockCov lf :=
  Nat.strongRecOn lf (fun n ih => covStep n ih)

/-! ## The coverage lemma -/

/-- Brief §3/§9 (1): a plan the wall lowers (hence one that passed
    `checkExprFragmentRawPlan`) whose nodes are in the profile relative to
    `env` has a typed body over the compute-face locals, and the body
    translates. The result sort is the result node's (`sortAt`), which is the
    plan's declared result sort or its literal refinement (`SubSort`). The
    carrier index must be declared in `env` with the fixed `{i64, ref, i32}`
    layout (`envOfClaim_carrier` for `envOfClaim`). -/
theorem coverage (env : TranslateEnv) (carrier : Nat)
    (hcar : structSorts? env.structs carrier = some [.i64, .ref, .i32])
    (plan : ExprFragmentRawPlan) (pf : Nat) (hprof : blockInProfile pf env plan.body = true)
    (instrs : List WInstr) (hlow : lowerExprFragmentBody carrier plan = some instrs) :
    ∃ t, SubSort t (sortOfFragTy plan.result) ∧
      HasTy env (Γof plan.params) [] instrs [t] ∧
      ∃ code, translateList env instrs = some code := by
  simp only [lowerExprFragmentBody] at hlow
  split at hlow
  · rename_i hchk
    simp only [checkExprFragmentRawPlan, Bool.and_eq_true] at hchk
    obtain ⟨⟨-, hblock⟩, hres⟩ := hchk
    simp only [lowerBlock] at hlow
    have hty : HasTy env (Γof plan.params) [] instrs [sortAt plan.body.nodes plan.body.result] :=
      (cov AverCert.PlanCheck.maxFuel).2 env carrier hcar plan.params AverCert.PlanCheck.maxFuel pf
        plan.body instrs hblock hprof hlow
    refine ⟨_, ?_, hty, HasTy_translates hty⟩
    split at hres
    · rename_i n hn
      rw [sortAt_of_lookupNode hn, ← sameTy_eq hres]
      exact sortOfNode_sub n
    · simp at hres
  · simp at hlow

/-- The same over the claim's projection `envOfClaim`. -/
theorem coverage_envOfClaim (hostTable : List (HostRole × Nat)) (carrier : Nat)
    (decls : List TypeDecl) (plan : ExprFragmentRawPlan)
    (hprof : planInProfile (envOfClaim hostTable carrier decls) plan = true)
    (instrs : List WInstr) (hlow : lowerExprFragmentBody carrier plan = some instrs) :
    ∃ t, SubSort t (sortOfFragTy plan.result) ∧
      HasTy (envOfClaim hostTable carrier decls) (Γof plan.params) [] instrs [t] ∧
      ∃ code, translateList (envOfClaim hostTable carrier decls) instrs = some code :=
  coverage _ carrier (envOfClaim_carrier hostTable carrier decls) plan AverCert.PlanCheck.maxFuel hprof
    instrs hlow

end Bridge
