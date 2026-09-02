/- The generic source-eval bridge for the record projection-compute face, v1
   node set (what the k5 gate needs):

     local / constI64 / constI32 / structGetUser / structNew /
     hostCall {box, add, sub, mul, cmp, eq} / prim {i32LtS, i32GtS, i32GeS} /
     intSignCmp

   over ONE user struct type whose fields are all Int carriers (Fraction).

   `sourceRunNodes` mirrors `ExprFragmentSemantics.runNodesFuel` ARM BY ARM —
   same fuel discipline, same symbolic-id stack, same popExpected/popExpectedAll
   plumbing — but computes over SOURCE values (ℤ, Bool, raw i64 literals, raw
   i32 verdicts, records-as-Int-lists). The agreement theorem walks both
   evaluators in lockstep: pointwise-SRepr stacks stay related at every step,
   host calls are bridged by the named box/add/sub/mul/cmp/eq contracts, and
   `structNew`/`structGetUser` are bridged by the record representation. The
   obligation's model for the generic face is `sourceRunBlock` — the plan IS
   the claim.

   The ONE node that writes a local is `intSignCmp`, the emitter's inline sign
   template; both evaluators write the same slot, so the locals lists stay
   pointwise related and the source locals carry a `pad` value for the declared
   scratch slot the wasm entry initialises to `null`. -/
import ExprFragmentSoundness

open CertPrelude AverCert.Schema AverCert.PlanLower ExprFragmentSemantics

namespace RecordComputeBridge

/-- Source-level values for the v1 face. `raw` is a bare i64 literal on its
    way into the `box` helper (the emitter's `i64.const k; call box` idiom) —
    distinct from `i`, which is a boxed source integer in the carrier. `i32` is
    a raw comparison verdict (`__aint_cmp`'s `-1`/`0`/`1` and the `i32.const 0`
    it is compared against), which is NOT a source Boolean. `pad` is the
    declared scratch local's initial `null`: it inhabits no source type and no
    admitted node can read it (a `local` node past the parameter prefix fails
    the typing face). -/
inductive SVal where
  | i (n : Int)
  | b (v : Bool)
  | i32 (n : Int)
  | raw (n : Int)
  | pad
  | r (fields : List Int)
deriving Repr

/-- A represented carrier word that is additionally in the runtime's normal
    form. Every carrier this face ever holds is canonical: parameters and
    record fields by the face's domain representation, box/add/sub/mul results
    by their contracts. Canonicity is what makes the two STRUCTURAL helpers
    (`__aint_cmp`, `__aint_eq`) and the inline sign template exact. -/
def CanonRepr {C : Nat} (S : CarrierSpec C) (n : Int) (w : WVal) : Prop :=
  S.Repr n w ∧ S.Canon w

/-- Representation of one source value by one wasm value, over the carrier
    specification `S` and the single user struct type `structIdx`. -/
def SRepr {C : Nat} (S : CarrierSpec C) (structIdx : Nat) : SVal → WVal → Prop
  | .i n, w => CanonRepr S n w
  | .b v, w => w = b32 v
  | .i32 n, w => w = .i32v n
  | .raw n, w => w = .i64v n ∧ -(2 ^ 63 : Int) ≤ n ∧ n < 2 ^ 63
  | .pad, w => w = .null
  | .r fields, w =>
      ∃ ws, w = .structv structIdx ws ∧ ReprAll (CanonRepr S) fields ws

/-- Pointwise representation of a source stack / locals list. -/
inductive SReprAll {C : Nat} (S : CarrierSpec C) (structIdx : Nat) :
    List SVal → List WVal → Prop where
  | nil : SReprAll S structIdx [] []
  | cons {sv w ss ws} : SRepr S structIdx sv w →
      SReprAll S structIdx ss ws →
      SReprAll S structIdx (sv :: ss) (w :: ws)

/-- Named host contracts of the v1 face — exactly the hypotheses
    `Obligation.holds` threads, at this face's concrete slots. `box` is the
    boxing helper's meaning (its body is byte-pinned, so at face level this is
    the synthesized semantics, not a new trust assumption); it is stated for an
    i64-band literal because that is the only literal the emitter can box, and
    that band is what `CarrierSpec.canonSmall` needs. -/
structure Contracts {C : Nat} (S : CarrierSpec C)
    (box add sub mul cmp eq : List WVal → Option WVal) : Prop where
  hBox : ∀ n w, -(2 ^ 63 : Int) ≤ n → n < 2 ^ 63 → box [.i64v n] = some w →
    CanonRepr S n w
  hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
    add [va, vb] = some w → CanonRepr S (a + b) w
  hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
    sub [va, vb] = some w → CanonRepr S (a - b) w
  hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
    mul [va, vb] = some w → CanonRepr S (a * b) w
  hCmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    cmp [va, vb] = some r → r = .i32v (cmpW a b)
  hEq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    eq [va, vb] = some r → r = .i32v (eqW a b)

/-- The wasm arity of each host role's signature. -/
def roleArity : HostRole → Nat
  | .box => 1
  | .toIndex => 1
  | _ => 2

/-- The contract function a used role denotes; the one role the v1 face never
    admits (`toIndex`) maps to the trap-only function. -/
def roleFn (box add sub mul cmp eq : List WVal → Option WVal) :
    HostRole → List WVal → Option WVal
  | .box => box
  | .add => add
  | .sub => sub
  | .mul => mul
  | .cmp => cmp
  | .eq => eq
  | .toIndex => fun _ => none

/-- The source meaning of one comparison operator against a literal. -/
def symIntCmpDenote : SymIntCmp → Int → Int → Bool
  | .eq, n, k => n = k
  | .lt, n, k => n < k
  | .le, n, k => n ≤ k
  | .ge, n, k => n ≥ k
  | .gt, n, k => n > k

/-- Which node kinds the v1 face admits, keyed on the byte-derived role
    TABLE: a host call is admitted only when the table resolves its role to
    exactly the cited function index. A role the table lacks fail-closes.
    Everything else is fail-closed. -/
def nodeAdmitted (hostTable : List (HostRole × Nat)) :
    FragNodeKind → Bool
  | .local _ => true
  -- The literal must be i64-representable: it is boxed through `canonSmall`,
  -- and the sign template's limb arm is only exact against a band literal.
  | .constI64 value => AverCert.PlanCheck.inI64Band value
  | .constI32 _ => true
  | .structGetUser _ _ _ => true
  | .structNew _ _ => true
  | .prim .i32LtS args => args.length == 2
  | .prim .i32GtS args => args.length == 2
  | .prim .i32GeS args => args.length == 2
  | .intSignCmp _ constant _ _ => AverCert.PlanCheck.inI64Band constant
  | .hostCall .box f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable .box == some f) &&
        args.length == 1
  | .hostCall .add f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable .add == some f) &&
        args.length == 2
  | .hostCall .sub f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable .sub == some f) &&
        args.length == 2
  | .hostCall .mul f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable .mul == some f) &&
        args.length == 2
  | .hostCall .cmp f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable .cmp == some f) &&
        args.length == 2
  | .hostCall .eq f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable .eq == some f) &&
        args.length == 2
  | _ => false

def nodesAdmitted (hostTable : List (HostRole × Nat))
    (nodes : List FragNode) : Bool :=
  nodes.all fun n => nodeAdmitted hostTable n.kind

/-- Pop `n` boxed integers off the source stack as record fields: the emitter
    pushes fields in declaration order, so the popped (reversed) prefix is
    reversed back. -/
def takeInts : Nat → List SVal → Option (List Int × List SVal)
  | 0, st => some ([], st)
  | n + 1, .i v :: st =>
      match takeInts n st with
      | some (vs, st') => some (vs ++ [v], st')
      | none => none
  | _ + 1, _ => none

/-- Source-level twin of `runNodesFuel`, arm by arm; fail-closed on every
    unadmitted shape. A `box` call converts a raw literal into a boxed source
    integer. -/
def sourceRunNodes :
    Nat → List FragNode → List Nat → List SVal → List SVal → Option (List SVal)
  | 0, _, _, _, _ => none
  | _fuel + 1, [], _, _, stack => some stack
  | fuel + 1, node :: rest, symStack, locals, stack =>
      match node.kind with
      | .local index =>
          match locals[index]? with
          | some v => sourceRunNodes fuel rest (node.id :: symStack) locals (v :: stack)
          | none => none
      | .constI64 value =>
          sourceRunNodes fuel rest (node.id :: symStack) locals (.raw value :: stack)
      | .constI32 value =>
          sourceRunNodes fuel rest (node.id :: symStack) locals (.i32 value :: stack)
      | .prim op args =>
          match popExpectedAll symStack args.reverse with
          | some symStack' =>
              match op, stack with
              | .i32LtS, .i32 b :: .i32 a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.b (a < b) :: stackRest)
              | .i32GtS, .i32 b :: .i32 a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.b (a > b) :: stackRest)
              | .i32GeS, .i32 b :: .i32 a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.b (a ≥ b) :: stackRest)
              | _, _ => none
          | none => none
      | .intSignCmp op k scratch value =>
          match popExpected symStack value, stack with
          | some symStack', .i n :: stackRest =>
              sourceRunNodes fuel rest (node.id :: symStack')
                (locals.set scratch (.i n))
                (.b (symIntCmpDenote op n k) :: stackRest)
          | _, _ => none
      | .structGetUser _tyIdx field value =>
          match popExpected symStack value, stack with
          | some symStack', .r fields :: stackRest =>
              match fields[field]? with
              | some n =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.i n :: stackRest)
              | none => none
          | _, _ => none
      | .structNew _tyIdx args =>
          match popExpectedAll symStack args.reverse with
          | some symStack' =>
              match takeInts args.length stack with
              | some (fields, stackRest) =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.r fields :: stackRest)
              | none => none
          | none => none
      | .hostCall role _f argIds =>
          match popExpectedAll symStack argIds.reverse with
          | some symStack' =>
              match role, stack with
              | .box, .raw n :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.i n :: stackRest)
              | .add, .i b :: .i a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.i (a + b) :: stackRest)
              | .sub, .i b :: .i a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.i (a - b) :: stackRest)
              | .mul, .i b :: .i a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.i (a * b) :: stackRest)
              | .cmp, .i b :: .i a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.i32 (cmpW a b) :: stackRest)
              | .eq, .i b :: .i a :: stackRest =>
                  sourceRunNodes fuel rest (node.id :: symStack') locals
                    (.b (a = b) :: stackRest)
              | _, _ => none
          | none => none
      | _ => none

/-- The v1 face's model: run the plan body over source inputs. Fuel is
    peeled exactly as `runBlockFuel` peels it, so the two block evaluators are
    twins level by level. -/
def sourceRunBlock : Nat → FragBlock → List SVal → Option SVal
  | 0, _, _ => none
  | fuel + 1, block, params =>
      -- The wasm entry runs with the one declared scratch local appended to
      -- the arguments (`initLocals`); the source locals mirror it with `pad`
      -- so the two locals lists stay pointwise related when `intSignCmp`
      -- writes that slot. No admitted node can READ it.
      match sourceRunNodes fuel block.nodes [] (params ++ [.pad]) [] with
      | some [v] => some v
      | _ => none

/-! ## Typing scaffold for the agreement proof

`agreement` is not provable for ill-typed plans: wasm `struct.new` packs ANY
operand values and an abstract host function may succeed on non-carrier
arguments, while the source evaluator demands boxed integers there, so the
source run can fail where the wasm run succeeds (e.g. `constI64` fed straight
into `structNew`). The repair threads the plan's type discipline — the v1
restriction of what `PlanCheck.checkBlockFuel` enforces on every accepted
plan: `tyOf` declares each node id's `FragTy`, `params` types the locals,
`nodeTyped` demands each admitted node consume/produce declared types, and
`IdsTyped` keeps the symbolic stack pointwise typed against the value
stacks. -/

/-- The `FragTy` a v1 source value inhabits. -/
def svalTy : SVal → FragTy
  | .i _ => .intCarrier
  | .b _ => .boolI32
  | .i32 _ => .rawI32
  | .raw _ => .i64
  -- No admitted node produces or consumes the scratch pad, so its type is
  -- never consulted; `.ref` is the type no plan node yields.
  | .pad => .ref
  | .r _ => .adtRef

/-- The symbolic stack and the source stack agree pointwise with the typing
    map: position `k` holds a value of the declared type of the id at `k`. -/
inductive IdsTyped (tyOf : Nat → FragTy) : List Nat → List SVal → Prop where
  | nil : IdsTyped tyOf [] []
  | cons {id : Nat} {sv : SVal} {ids : List Nat} {svs : List SVal} :
      svalTy sv = tyOf id → IdsTyped tyOf ids svs →
      IdsTyped tyOf (id :: ids) (sv :: svs)

/-- One node's typing discipline (v1 subset of `PlanCheck.checkBlockFuel`).
    `structNew` additionally pins its type index to the ONE user struct type;
    `structGetUser` needs no pin — the wasm struct-tag check forces it. -/
def nodeTyped (structIdx : Nat) (tyOf : Nat → FragTy)
    (params : List FragTy) (node : FragNode) : Prop :=
  match node.kind with
  | .local index => params[index]? = some (tyOf node.id)
  | .constI64 _ => tyOf node.id = .i64
  | .constI32 _ => tyOf node.id = .rawI32
  | .structGetUser _ _ value =>
      tyOf value = .adtRef ∧ tyOf node.id = .intCarrier
  | .structNew tyIdx args =>
      tyIdx = structIdx ∧ (∀ a ∈ args, tyOf a = .intCarrier) ∧
        tyOf node.id = .adtRef
  | .prim _ args =>
      (∀ a ∈ args, tyOf a = .rawI32) ∧ tyOf node.id = .boolI32
  | .hostCall .box _ args =>
      (∀ a ∈ args, tyOf a = .i64) ∧ tyOf node.id = .intCarrier
  | .hostCall .cmp _ args =>
      (∀ a ∈ args, tyOf a = .intCarrier) ∧ tyOf node.id = .rawI32
  | .hostCall .eq _ args =>
      (∀ a ∈ args, tyOf a = .intCarrier) ∧ tyOf node.id = .boolI32
  | .hostCall _ _ args =>
      (∀ a ∈ args, tyOf a = .intCarrier) ∧ tyOf node.id = .intCarrier
  -- The scratch slot is pinned PAST the parameter prefix, so the template
  -- can never clobber a parameter local — the lockstep locals invariant of
  -- `agreement` rests on exactly that.
  | .intSignCmp _ _ scratch value =>
      tyOf value = .intCarrier ∧ scratch = params.length ∧
        tyOf node.id = .boolI32
  | _ => True

def planTyped (structIdx : Nat) (tyOf : Nat → FragTy)
    (params : List FragTy) : List FragNode → Prop
  | [] => True
  | node :: rest =>
      nodeTyped structIdx tyOf params node ∧ planTyped structIdx tyOf params rest

private theorem svalTy_int {sv : SVal} (h : svalTy sv = .intCarrier) :
    ∃ n, sv = .i n := by
  cases sv <;> first | exact ⟨_, rfl⟩ | simp [svalTy] at h

private theorem svalTy_i64 {sv : SVal} (h : svalTy sv = .i64) :
    ∃ n, sv = .raw n := by
  cases sv <;> first | exact ⟨_, rfl⟩ | simp [svalTy] at h

private theorem svalTy_adt {sv : SVal} (h : svalTy sv = .adtRef) :
    ∃ fields, sv = .r fields := by
  cases sv <;> first | exact ⟨_, rfl⟩ | simp [svalTy] at h

private theorem idsTyped_cons_inv {tyOf : Nat → FragTy} {id : Nat}
    {ids : List Nat} {svs : List SVal}
    (h : IdsTyped tyOf (id :: ids) svs) :
    ∃ sv svs', svs = sv :: svs' ∧ svalTy sv = tyOf id ∧
      IdsTyped tyOf ids svs' := by
  cases h with
  | cons h1 h2 => exact ⟨_, _, rfl, h1, h2⟩

private theorem sreprAll_cons_inv {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {sv : SVal} {svs : List SVal} {ws : List WVal}
    (h : SReprAll S structIdx (sv :: svs) ws) :
    ∃ w ws', ws = w :: ws' ∧ SRepr S structIdx sv w ∧
      SReprAll S structIdx svs ws' := by
  cases h with
  | cons h1 h2 => exact ⟨_, _, rfl, h1, h2⟩

private theorem sreprAll_length {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {ss : List SVal} {ws : List WVal}
    (h : SReprAll S structIdx ss ws) : ss.length = ws.length := by
  induction h with
  | nil => rfl
  | cons _ _ ih => simp [ih]

private theorem sreprAll_getElem? {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {ss : List SVal} {ws : List WVal}
    (h : SReprAll S structIdx ss ws) {i : Nat} {wv : WVal}
    (hw : ws[i]? = some wv) :
    ∃ sv, ss[i]? = some sv ∧ SRepr S structIdx sv wv := by
  induction h generalizing i with
  | nil => simp at hw
  | cons hsv _ ih =>
      cases i with
      | zero =>
          simp only [List.getElem?_cons_zero, Option.some.injEq] at hw
          exact ⟨_, rfl, hw ▸ hsv⟩
      | succ i =>
          simp only [List.getElem?_cons_succ] at hw ⊢
          exact ih hw

private theorem reprAll_getElem? {Repr : Int → WVal → Prop}
    {ns : List Int} {vs : List WVal}
    (h : ReprAll Repr ns vs) {i : Nat} {wv : WVal}
    (hw : vs[i]? = some wv) :
    ∃ m, ns[i]? = some m ∧ Repr m wv := by
  induction h generalizing i with
  | nil => simp at hw
  | cons hn _ ih =>
      cases i with
      | zero =>
          simp only [List.getElem?_cons_zero, Option.some.injEq] at hw
          exact ⟨_, rfl, hw ▸ hn⟩
      | succ i =>
          simp only [List.getElem?_cons_succ] at hw ⊢
          exact ih hw

private theorem reprAll_append_single {Repr : Int → WVal → Prop}
    {ns : List Int} {vs : List WVal} (h : ReprAll Repr ns vs)
    {m : Int} {wv : WVal} (hm : Repr m wv) :
    ReprAll Repr (ns ++ [m]) (vs ++ [wv]) := by
  induction h with
  | nil => exact .cons hm .nil
  | cons hx _ ih => exact .cons hx ih

private theorem popExpected_eq {symStack : List Nat} {v : Nat} {s' : List Nat}
    (h : popExpected symStack v = some s') : symStack = v :: s' := by
  cases symStack with
  | nil => simp [popExpected] at h
  | cons got r =>
      by_cases hg : got = v
      · subst hg
        simp [popExpected] at h
        simp [h]
      · simp [popExpected, hg] at h

private theorem popExpectedAll_append {ids : List Nat} :
    ∀ {symStack symRest : List Nat},
      popExpectedAll symStack ids = some symRest →
      symStack = ids ++ symRest := by
  induction ids with
  | nil =>
      intro symStack symRest h
      simp only [popExpectedAll, Option.some.injEq] at h
      simp [h]
  | cons e rest ih =>
      intro symStack symRest h
      simp only [popExpectedAll] at h
      cases hp : popExpected symStack e with
      | none => simp [hp] at h
      | some s' =>
          simp only [hp] at h
          rw [popExpected_eq hp, ih h]
          rfl

private theorem idsTyped_length {tyOf : Nat → FragTy} {ids : List Nat}
    {svs : List SVal} (h : IdsTyped tyOf ids svs) :
    svs.length = ids.length := by
  induction h with
  | nil => rfl
  | cons _ _ ih => simp [ih]

private theorem idsTyped_split {tyOf : Nat → FragTy} {ids1 : List Nat} :
    ∀ {ids2 : List Nat} {svs : List SVal},
      IdsTyped tyOf (ids1 ++ ids2) svs →
      ∃ svs1 svs2, svs = svs1 ++ svs2 ∧ IdsTyped tyOf ids1 svs1 ∧
        IdsTyped tyOf ids2 svs2 := by
  induction ids1 with
  | nil =>
      intro ids2 svs h
      exact ⟨[], svs, rfl, .nil, h⟩
  | cons id ids ih =>
      intro ids2 svs h
      obtain ⟨sv, svs', rfl, h1, h2⟩ := idsTyped_cons_inv h
      obtain ⟨svs1, svs2, rfl, h3, h4⟩ := ih h2
      exact ⟨sv :: svs1, svs2, rfl, .cons h1 h3, h4⟩

private theorem sreprAll_split {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {ss1 : List SVal} :
    ∀ {ss2 : List SVal} {ws : List WVal},
      SReprAll S structIdx (ss1 ++ ss2) ws →
      ∃ ws1 ws2, ws = ws1 ++ ws2 ∧ SReprAll S structIdx ss1 ws1 ∧
        SReprAll S structIdx ss2 ws2 := by
  induction ss1 with
  | nil =>
      intro ss2 ws h
      exact ⟨[], ws, rfl, .nil, h⟩
  | cons sv ss ih =>
      intro ss2 ws h
      obtain ⟨w, ws', rfl, h1, h2⟩ := sreprAll_cons_inv h
      obtain ⟨ws1, ws2, rfl, h3, h4⟩ := ih h2
      exact ⟨w :: ws1, ws2, rfl, .cons h1 h3, h4⟩

private theorem popArgs_append {ws1 wRest : List WVal} :
    popArgs ws1.length (ws1 ++ wRest) = some (ws1.reverse, wRest) := by
  simp [popArgs, List.take_append, List.drop_append]

private theorem srepr_rec {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {fields : List Int} {ws : List WVal} (h : ReprAll (CanonRepr S) fields ws) :
    SRepr S structIdx (.r fields) (.structv structIdx ws) :=
  ⟨ws, rfl, h⟩

private theorem takeInts_bridge {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {tyOf : Nat → FragTy} {ids : List Nat} {svs : List SVal}
    (hty : IdsTyped tyOf ids svs) :
    (∀ a ∈ ids, tyOf a = .intCarrier) →
    ∀ {ws : List WVal}, SReprAll S structIdx svs ws →
      ∀ sRest : List SVal,
        ∃ fields, takeInts ids.length (svs ++ sRest) = some (fields, sRest) ∧
          ReprAll (CanonRepr S) fields ws.reverse := by
  induction hty with
  | nil =>
      intro _ ws hrel sRest
      cases hrel
      exact ⟨[], rfl, .nil⟩
  | cons hsv htail ih =>
      intro hint ws hrel sRest
      rename_i id sv ids' svs'
      obtain ⟨n, rfl⟩ := svalTy_int (hsv.trans (hint id (by simp)))
      obtain ⟨w, ws', rfl, h1, h2⟩ := sreprAll_cons_inv hrel
      have hR : CanonRepr S n w := h1
      obtain ⟨fields, htake, hra⟩ :=
        ih (fun a ha => hint a (List.mem_cons_of_mem _ ha)) h2 sRest
      refine ⟨fields ++ [n], ?_, ?_⟩
      · simp only [List.length_cons, List.cons_append, takeInts, htake]
      · simpa [List.reverse_cons] using reprAll_append_single hra hR

/-! ## Agreement

The wasm-side evaluator and the source-side evaluator stay pointwise-SRepr
related at every step. Exactly one admitted node writes a local — the inline
sign template, which stashes its operand in the DECLARED SCRATCH slot — and
both evaluators write the same slot with related values, so the locals lists
stay related too. No admitted node returns early, which is why the conclusion
can pin the run's output to an `.ok`. -/

/-- Writing index `i` of a list and reading it back, in range. -/
private theorem setSelf? {α : Type _} (a : α) :
    ∀ (l : List α) (i : Nat), i < l.length → (l.set i a)[i]? = some a := by
  intro l
  induction l with
  | nil => intro i h; simp at h
  | cons x xs ih =>
      intro i h
      cases i with
      | zero => rfl
      | succ i => exact ih i (by simpa using h)

/-- Writing index `i` leaves every other index alone. -/
private theorem setNe? {α : Type _} (a : α) :
    ∀ (l : List α) (i j : Nat), i ≠ j → (l.set i a)[j]? = l[j]? := by
  intro l
  induction l with
  | nil => intro i j _; simp
  | cons x xs ih =>
      intro i j hne
      cases i with
      | zero =>
          cases j with
          | zero => exact absurd rfl hne
          | succ j => rfl
      | succ i =>
          cases j with
          | zero => rfl
          | succ j => exact ih i j (fun h => hne (by omega))

/-- Pointwise representation survives a write of related values at one index. -/
private theorem sreprAll_set {C : Nat} {S : CarrierSpec C} {structIdx : Nat}
    {sv : SVal} {w : WVal} (hsv : SRepr S structIdx sv w) :
    ∀ {ss : List SVal} {ws : List WVal}, SReprAll S structIdx ss ws →
      ∀ (i : Nat), SReprAll S structIdx (ss.set i sv) (ws.set i w) := by
  intro ss ws h
  induction h with
  | nil => intro _; exact .nil
  | cons h1 h2 ih =>
      intro i
      cases i with
      | zero => exact .cons hsv h2
      | succ i => exact .cons h1 (ih i)

private theorem sreprAll_append {C : Nat} {S : CarrierSpec C} {structIdx : Nat} :
    ∀ {ss1 ws1 ss2 ws2 : _}, SReprAll S structIdx ss1 ws1 →
      SReprAll S structIdx ss2 ws2 →
      SReprAll S structIdx (ss1 ++ ss2) (ws1 ++ ws2) := by
  intro ss1 ws1 ss2 ws2 h1 h2
  induction h1 with
  | nil => exact h2
  | cons hx _ ih => exact .cons hx ih

private theorem svalTy_i32 {sv : SVal} (h : svalTy sv = .rawI32) :
    ∃ n, sv = .i32 n := by
  cases sv <;> first | exact ⟨_, rfl⟩ | simp [svalTy] at h

private theorem eqW_b32 (a b : Int) : WVal.i32v (eqW a b) = b32 (a = b) := by
  by_cases h : a = b <;> simp [eqW, b32, h]

/-- The whole inline sign template, evaluated. The operand is stashed in the
    scratch local; the `limbs = null` test picks the native i64 compare of the
    `small` field (exact by `smallElim`) or the sign-only decision, which is
    exact because a CANONICAL limb-carrying carrier lies outside the i64 band
    the literal lives in (`canonBig`) while its sign tracks the value's sign
    (`bigElim`). -/
private theorem intSignCmp_step {C : Nat} (S : CarrierSpec C)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (op : SymIntCmp) (k : Int) (scratch : Nat)
    (locals stack : List WVal) (n : Int) (w : WVal) (out : Out)
    (hband : AverCert.PlanCheck.inI64Band k = true)
    (hRepr : S.Repr n w) (hCanon : S.Canon w)
    (hrun : wRunF host ar callee (intSignCmpTemplate C scratch op k) locals
      (w :: stack) = some out) :
    out = .ok (locals.set scratch w)
      (b32 (symIntCmpDenote op n k) :: stack) := by
  have hk : -(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63 := by
    simpa [AverCert.PlanCheck.inI64Band, Bool.and_eq_true, decide_eq_true_eq]
      using hband
  by_cases hlt : scratch < locals.length
  · have hget : (locals.set scratch w)[scratch]? = some w :=
      setSelf? w locals scratch hlt
    rcases S.car n w hRepr with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
    · have hs : s = n := S.smallElim n s sg hRepr
      subst hs
      cases op <;>
        · simp [intSignCmpTemplate, intSignCmpBigArm, intSignCmpSmallPrim,
            primInstr, wRunF, hget, b32] at hrun
          subst hrun
          simp [symIntCmpDenote, b32]
    · obtain ⟨hnb, hsgne⟩ := S.canonBig n s lty les sg hRepr hCanon
      obtain ⟨hsign, _hnz⟩ := S.bigElim n s lty les sg hRepr
      have hcase : n < -(2 ^ 63 : Int) ∨ (2 ^ 63 : Int) ≤ n := by omega
      have hLtIff : (sg < 0) ↔ n < k := by
        constructor
        · intro h; have := hsign.mp h; omega
        · intro h; exact hsign.mpr (by omega)
      have hGtIff : (0 < sg) ↔ k < n := by
        constructor
        · intro h
          have hnn : ¬ n < 0 := by intro hc; have := hsign.mpr hc; omega
          omega
        · intro h
          have hnn : ¬ sg < 0 := by intro hc; have := hsign.mp hc; omega
          omega
      have hNe : ¬ n = k := by
        intro he; subst he; omega
      have hLe : (n ≤ k) = (n < k) := by
        simp only [eq_iff_iff]
        constructor
        · intro h; omega
        · intro h; omega
      have hGe : (k ≤ n) = (k < n) := by
        simp only [eq_iff_iff]
        constructor
        · intro h; omega
        · intro h; omega
      cases op <;>
        · simp [intSignCmpTemplate, intSignCmpBigArm, intSignCmpSmallPrim,
            primInstr, wRunF, hget, b32] at hrun
          subst hrun
          simp [symIntCmpDenote, b32, hLtIff, hGtIff, hNe, hLe, hGe]
  · have hget : (locals.set scratch w)[scratch]? = none := by
      apply List.getElem?_eq_none
      simpa using Nat.le_of_not_lt hlt
    simp [intSignCmpTemplate, wRunF, hget] at hrun

/- STATEMENT ADJUSTMENT (authorized, documented): as originally stated the
   theorem is unprovable for ill-typed plans — wasm `struct.new` packs ANY
   operand values and an abstract host function may succeed on non-carrier
   arguments, while the source evaluator demands boxed integers there, so the
   source run can fail where the wasm run succeeds (e.g. a `constI64` result
   fed straight into `structNew`, or a record fed to `add`). The minimal
   repair threads the plan's type discipline, which `PlanCheck.checkBlockFuel`
   enforces on every accepted plan: added hypotheses `hTy` (each admitted node
   consumes/produces its declared `FragTy`, pinning `structNew`'s type index to
   the ONE user struct type and the sign template's scratch slot PAST the
   parameter prefix — nothing on the wasm side forces either), `hLocalsTy`
   (parameter locals inhabit `params`), and `hStackTy` (the symbolic stack
   stays pointwise typed). `structGetUser`'s type index needs no pin: the wasm
   struct-tag check forces `tyIdx = structIdx` on any successful run. -/
theorem agreement
    {C : Nat} (S : CarrierSpec C) (structIdx : Nat)
    (box add sub mul cmp eq : List WVal → Option WVal)
    (Ctr : Contracts S box add sub mul cmp eq)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat))
    (hHost : ∀ role idx,
      role ∈ [HostRole.box, HostRole.add, HostRole.sub, HostRole.mul,
        HostRole.cmp, HostRole.eq] →
      AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx →
      host idx = some (roleArity role, roleFn box add sub mul cmp eq role))
    (tyOf : Nat → FragTy) (params : List FragTy) :
    ∀ (fuel : Nat) (nodes : List FragNode) (symStack : List Nat)
      (sLocals : List SVal) (wLocals : List WVal)
      (sStack : List SVal) (wStack : List WVal) (out : Out),
      nodesAdmitted hostTable nodes = true →
      planTyped structIdx tyOf params nodes →
      (∀ (i : Nat) (sv : SVal), i < params.length →
        sLocals[i]? = some sv → params[i]? = some (svalTy sv)) →
      IdsTyped tyOf symStack sStack →
      SReprAll S structIdx sLocals wLocals →
      SReprAll S structIdx sStack wStack →
      runNodesFuel host ar callee fuel C nodes symStack wLocals wStack
        = some out →
      ∃ wLocals' wStack' sStack',
        out = .ok wLocals' wStack' ∧
        sourceRunNodes fuel nodes symStack sLocals sStack = some sStack' ∧
        SReprAll S structIdx sStack' wStack' := by
  intro fuel
  induction fuel with
  | zero =>
      intro nodes symStack sLocals wLocals sStack wStack out _ _ _ _ _ _ hrun
      simp [runNodesFuel] at hrun
  | succ fuel ih =>
      intro nodes symStack sLocals wLocals sStack wStack out hAdm hTy hLocalsTy
        hStackTy hLocals hStack hrun
      cases nodes with
      | nil =>
          simp only [runNodesFuel, Option.some.injEq] at hrun
          exact ⟨wLocals, wStack, sStack, hrun.symm, by simp [sourceRunNodes],
            hStack⟩
      | cons node rest =>
          have hAdmPair : nodeAdmitted hostTable node.kind = true ∧
              nodesAdmitted hostTable rest = true := by
            simpa [nodesAdmitted, List.all_cons] using hAdm
          obtain ⟨hAdmN, hAdmR⟩ := hAdmPair
          simp only [planTyped] at hTy
          obtain ⟨hTyN, hTyR⟩ := hTy
          cases hk : node.kind
          case «local» index =>
            simp only [nodeTyped, hk] at hTyN
            simp only [runNodesFuel, hk] at hrun
            cases hl : wLocals[index]? with
            | none => simp [wRunF, hl] at hrun
            | some wv =>
                have hwr : wRunF host ar callee [.localGet index] wLocals wStack
                    = some (.ok wLocals (wv :: wStack)) := by
                  simp [wRunF, hl]
                simp only [hwr] at hrun
                obtain ⟨sv, hsl, hsv⟩ := sreprAll_getElem? hLocals hl
                have hidx : index < params.length := by
                  rcases Nat.lt_or_ge index params.length with h | h
                  · exact h
                  · rw [List.getElem?_eq_none h] at hTyN; simp at hTyN
                have hty : svalTy sv = tyOf node.id :=
                  Option.some.inj ((hLocalsTy index sv hidx hsl).symm.trans hTyN)
                obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                  ih rest (node.id :: symStack) sLocals wLocals (sv :: sStack)
                    (wv :: wStack) out hAdmR hTyR hLocalsTy
                    (.cons hty hStackTy) hLocals (.cons hsv hStack) hrun
                exact ⟨wl, ws, sStack', hout,
                  by simp [sourceRunNodes, hk, hsl, hsrc], hrel⟩
          case constBool value =>
            simp [nodeAdmitted, hk] at hAdmN
          case constI64 value =>
            simp only [nodeTyped, hk] at hTyN
            rw [hk] at hAdmN
            simp only [nodeAdmitted] at hAdmN
            have hband : -(2 ^ 63 : Int) ≤ value ∧ value < 2 ^ 63 := by
              simpa [AverCert.PlanCheck.inI64Band, Bool.and_eq_true,
                decide_eq_true_eq] using hAdmN
            simp only [runNodesFuel, hk] at hrun
            have hwr : wRunF host ar callee [.i64Const value] wLocals wStack
                = some (.ok wLocals (.i64v value :: wStack)) := by
              simp [wRunF]
            simp only [hwr] at hrun
            obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
              ih rest (node.id :: symStack) sLocals wLocals
                (.raw value :: sStack) (.i64v value :: wStack) out
                hAdmR hTyR hLocalsTy (.cons (by simp [svalTy, hTyN]) hStackTy)
                hLocals (.cons ⟨rfl, hband.1, hband.2⟩ hStack) hrun
            exact ⟨wl, ws, sStack', hout,
              by simp [sourceRunNodes, hk, hsrc], hrel⟩
          case constI32 value =>
            simp only [nodeTyped, hk] at hTyN
            simp only [runNodesFuel, hk] at hrun
            have hwr : wRunF host ar callee [.i32Const value] wLocals wStack
                = some (.ok wLocals (.i32v value :: wStack)) := by
              simp [wRunF]
            simp only [hwr] at hrun
            obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
              ih rest (node.id :: symStack) sLocals wLocals
                (.i32 value :: sStack) (.i32v value :: wStack) out
                hAdmR hTyR hLocalsTy (.cons (by simp [svalTy, hTyN]) hStackTy)
                hLocals (.cons rfl hStack) hrun
            exact ⟨wl, ws, sStack', hout,
              by simp [sourceRunNodes, hk, hsrc], hrel⟩
          case constF64Bits bits =>
            simp [nodeAdmitted, hk] at hAdmN
          case structGet field receiver =>
            simp [nodeAdmitted, hk] at hAdmN
          case structGetUser tyIdx field value =>
            simp only [nodeTyped, hk] at hTyN
            obtain ⟨hTyV, hTyId⟩ := hTyN
            simp only [runNodesFuel, hk] at hrun
            cases hp : popExpected symStack value with
            | none => simp [hp] at hrun
            | some symRest =>
                simp only [hp] at hrun
                have hsym := popExpected_eq hp
                subst hsym
                obtain ⟨sv, svs, rfl, hty1, hStackTy'⟩ :=
                  idsTyped_cons_inv hStackTy
                obtain ⟨fields, rfl⟩ := svalTy_adt (hty1.trans hTyV)
                obtain ⟨w, ws0, rfl, hsv, hStack'⟩ := sreprAll_cons_inv hStack
                have hsv' : ∃ wsf, w = .structv structIdx wsf ∧
                    ReprAll (CanonRepr S) fields wsf := hsv
                obtain ⟨wsf, rfl, hra⟩ := hsv'
                by_cases hti : structIdx = tyIdx
                · subst hti
                  cases hf : wsf[field]? with
                  | none => simp [wRunF, hf] at hrun
                  | some wv =>
                      have hwr : wRunF host ar callee
                          [.structGet structIdx field] wLocals
                          (.structv structIdx wsf :: ws0)
                          = some (.ok wLocals (wv :: ws0)) := by
                        simp [wRunF, hf]
                      simp only [hwr] at hrun
                      obtain ⟨m, hfm, hRm⟩ := reprAll_getElem? hra hf
                      obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                        ih rest (node.id :: symRest) sLocals wLocals
                          (.i m :: svs) (wv :: ws0) out hAdmR hTyR hLocalsTy
                          (.cons (by simp [svalTy, hTyId]) hStackTy') hLocals
                          (.cons hRm hStack') hrun
                      exact ⟨wl, ws, sStack', hout,
                        by simp [sourceRunNodes, hk, popExpected, hfm, hsrc],
                        hrel⟩
                · simp [wRunF, hti] at hrun
          case refIsNull value =>
            simp [nodeAdmitted, hk] at hAdmN
          case prim op args =>
            rw [hk] at hAdmN
            simp only [nodeTyped, hk] at hTyN
            obtain ⟨hArgTy, hTyId⟩ := hTyN
            simp only [runNodesFuel, hk] at hrun
            cases hp : popExpectedAll symStack args.reverse with
            | none => simp [hp] at hrun
            | some symRest =>
                simp only [hp] at hrun
                have hsym := popExpectedAll_append hp
                have hlen : args.length = 2 := by
                  cases op <;>
                    simp only [nodeAdmitted] at hAdmN <;>
                    simpa using hAdmN
                cases args with
                | nil => simp at hlen
                | cons a1 t =>
                    cases t with
                    | nil => simp at hlen
                    | cons a2 t2 =>
                        cases t2 with
                        | cons a3 t3 =>
                            simp only [List.length_cons] at hlen; omega
                        | nil =>
                            simp at hsym
                            subst hsym
                            obtain ⟨sv2, svsA, rfl, hty2, hStackTyA⟩ :=
                              idsTyped_cons_inv hStackTy
                            obtain ⟨sv1, svs, rfl, hty1, hStackTy'⟩ :=
                              idsTyped_cons_inv hStackTyA
                            obtain ⟨x2, rfl⟩ := svalTy_i32
                              (hty2.trans (hArgTy a2 (by simp)))
                            obtain ⟨x1, rfl⟩ := svalTy_i32
                              (hty1.trans (hArgTy a1 (by simp)))
                            obtain ⟨wv2, wsA, rfl, hsv2, hStackA⟩ :=
                              sreprAll_cons_inv hStack
                            obtain ⟨wv1, ws0, rfl, hsv1, hStack'⟩ :=
                              sreprAll_cons_inv hStackA
                            have hw2 : wv2 = WVal.i32v x2 := hsv2
                            have hw1 : wv1 = WVal.i32v x1 := hsv1
                            subst hw1
                            subst hw2
                            cases op
                            case i32LtS =>
                              have hwr : wRunF host ar callee
                                  [primInstr .i32LtS] wLocals
                                  (WVal.i32v x2 :: WVal.i32v x1 :: ws0)
                                  = some (.ok wLocals (b32 (x1 < x2) :: ws0)) := by
                                simp [wRunF, primInstr]
                              simp only [hwr] at hrun
                              obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                ih rest (node.id :: symRest) sLocals wLocals
                                  (.b (x1 < x2) :: svs) (b32 (x1 < x2) :: ws0)
                                  out hAdmR hTyR hLocalsTy
                                  (.cons (by simp [svalTy, hTyId]) hStackTy')
                                  hLocals (.cons rfl hStack') hrun
                              exact ⟨wl, ws, sStack', hout,
                                by simp [sourceRunNodes, hk, popExpectedAll,
                                  popExpected, hsrc], hrel⟩
                            case i32GtS =>
                              have hwr : wRunF host ar callee
                                  [primInstr .i32GtS] wLocals
                                  (WVal.i32v x2 :: WVal.i32v x1 :: ws0)
                                  = some (.ok wLocals (b32 (x1 > x2) :: ws0)) := by
                                simp [wRunF, primInstr]
                              simp only [hwr] at hrun
                              obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                ih rest (node.id :: symRest) sLocals wLocals
                                  (.b (x1 > x2) :: svs) (b32 (x1 > x2) :: ws0)
                                  out hAdmR hTyR hLocalsTy
                                  (.cons (by simp [svalTy, hTyId]) hStackTy')
                                  hLocals (.cons rfl hStack') hrun
                              exact ⟨wl, ws, sStack', hout,
                                by simp [sourceRunNodes, hk, popExpectedAll,
                                  popExpected, hsrc], hrel⟩
                            case i32GeS =>
                              have hwr : wRunF host ar callee
                                  [primInstr .i32GeS] wLocals
                                  (WVal.i32v x2 :: WVal.i32v x1 :: ws0)
                                  = some (.ok wLocals (b32 (x1 ≥ x2) :: ws0)) := by
                                simp [wRunF, primInstr]
                              simp only [hwr] at hrun
                              obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                ih rest (node.id :: symRest) sLocals wLocals
                                  (.b (x1 ≥ x2) :: svs) (b32 (x1 ≥ x2) :: ws0)
                                  out hAdmR hTyR hLocalsTy
                                  (.cons (by simp [svalTy, hTyId]) hStackTy')
                                  hLocals (.cons rfl hStack') hrun
                              exact ⟨wl, ws, sStack', hout,
                                by simp [sourceRunNodes, hk, popExpectedAll,
                                  popExpected, hsrc], hrel⟩
                            all_goals exact absurd hAdmN (by simp [nodeAdmitted])
          case hostCall role f args =>
            simp only [runNodesFuel, hk] at hrun
            cases hp : popExpectedAll symStack args.reverse with
            | none => simp [hp] at hrun
            | some symRest =>
                simp only [hp] at hrun
                have hsym := popExpectedAll_append hp
                rw [hk] at hAdmN
                cases role with
                | toIndex => simp [nodeAdmitted] at hAdmN
                | box =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostBox : host f = some (1, box) :=
                      hHost .box f (by simp) hfEq
                    simp only [nodeTyped, hk] at hTyN
                    obtain ⟨hArgTy, hTyId⟩ := hTyN
                    cases args with
                    | nil => simp at hlen
                    | cons a t =>
                        cases t with
                        | cons b t2 => simp only [List.length_cons] at hlen; omega
                        | nil =>
                            simp at hsym
                            subst hsym
                            obtain ⟨sv, svs, rfl, hty1, hStackTy'⟩ :=
                              idsTyped_cons_inv hStackTy
                            obtain ⟨n, rfl⟩ := svalTy_i64
                              (hty1.trans (hArgTy a (by simp)))
                            obtain ⟨w, ws0, rfl, hsv, hStack'⟩ :=
                              sreprAll_cons_inv hStack
                            obtain ⟨hw, hlo, hhi⟩ := hsv
                            subst hw
                            have hpa : popArgs 1 (WVal.i64v n :: ws0)
                                = some ([WVal.i64v n], ws0) := by
                              simpa using popArgs_append
                                (ws1 := [WVal.i64v n]) (wRest := ws0)
                            cases hb : box [.i64v n] with
                            | none => simp [wRunF, hHostBox, hpa, hb] at hrun
                            | some r =>
                                have hwr : wRunF host ar callee [.call f]
                                    wLocals (.i64v n :: ws0)
                                    = some (.ok wLocals (r :: ws0)) := by
                                  simp [wRunF, hHostBox, hpa, hb]
                                simp only [hwr] at hrun
                                obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                  ih rest (node.id :: symRest) sLocals wLocals
                                    (.i n :: svs) (r :: ws0) out hAdmR hTyR
                                    hLocalsTy
                                    (.cons (by simp [svalTy, hTyId]) hStackTy')
                                    hLocals
                                    (.cons (Ctr.hBox n r hlo hhi hb) hStack')
                                    hrun
                                exact ⟨wl, ws, sStack', hout,
                                  by simp [sourceRunNodes, hk, popExpectedAll,
                                    popExpected, hsrc], hrel⟩
                | add =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostFn : host f = some (2, add) :=
                      hHost .add f (by simp) hfEq
                    simp only [nodeTyped, hk] at hTyN
                    obtain ⟨hArgTy, hTyId⟩ := hTyN
                    cases args with
                    | nil => simp at hlen
                    | cons a1 t =>
                        cases t with
                        | nil => simp at hlen
                        | cons a2 t2 =>
                            cases t2 with
                            | cons a3 t3 =>
                                simp only [List.length_cons] at hlen; omega
                            | nil =>
                                simp at hsym
                                subst hsym
                                obtain ⟨sv2, svsA, rfl, hty2, hStackTyA⟩ :=
                                  idsTyped_cons_inv hStackTy
                                obtain ⟨sv1, svs, rfl, hty1, hStackTy'⟩ :=
                                  idsTyped_cons_inv hStackTyA
                                obtain ⟨x2, rfl⟩ := svalTy_int
                                  (hty2.trans (hArgTy a2 (by simp)))
                                obtain ⟨x1, rfl⟩ := svalTy_int
                                  (hty1.trans (hArgTy a1 (by simp)))
                                obtain ⟨wv2, wsA, rfl, hsv2, hStackA⟩ :=
                                  sreprAll_cons_inv hStack
                                obtain ⟨wv1, ws0, rfl, hsv1, hStack'⟩ :=
                                  sreprAll_cons_inv hStackA
                                have hR2 : CanonRepr S x2 wv2 := hsv2
                                have hR1 : CanonRepr S x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : add [wv1, wv2] with
                                | none => simp [wRunF, hHostFn, hpa, hb] at hrun
                                | some r =>
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals (r :: ws0)) := by
                                      simp [wRunF, hHostFn, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                      ih rest (node.id :: symRest) sLocals wLocals
                                        (.i (x1 + x2) :: svs) (r :: ws0) out hAdmR
                                        hTyR hLocalsTy
                                        (.cons (by simp [svalTy, hTyId]) hStackTy')
                                        hLocals
                                        (.cons (Ctr.hAdd x1 x2 wv1 wv2 r hR1.1
                                          hR2.1 hb) hStack') hrun
                                    exact ⟨wl, ws, sStack', hout,
                                      by simp [sourceRunNodes, hk, popExpectedAll,
                                        popExpected, hsrc], hrel⟩
                | sub =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostFn : host f = some (2, sub) :=
                      hHost .sub f (by simp) hfEq
                    simp only [nodeTyped, hk] at hTyN
                    obtain ⟨hArgTy, hTyId⟩ := hTyN
                    cases args with
                    | nil => simp at hlen
                    | cons a1 t =>
                        cases t with
                        | nil => simp at hlen
                        | cons a2 t2 =>
                            cases t2 with
                            | cons a3 t3 =>
                                simp only [List.length_cons] at hlen; omega
                            | nil =>
                                simp at hsym
                                subst hsym
                                obtain ⟨sv2, svsA, rfl, hty2, hStackTyA⟩ :=
                                  idsTyped_cons_inv hStackTy
                                obtain ⟨sv1, svs, rfl, hty1, hStackTy'⟩ :=
                                  idsTyped_cons_inv hStackTyA
                                obtain ⟨x2, rfl⟩ := svalTy_int
                                  (hty2.trans (hArgTy a2 (by simp)))
                                obtain ⟨x1, rfl⟩ := svalTy_int
                                  (hty1.trans (hArgTy a1 (by simp)))
                                obtain ⟨wv2, wsA, rfl, hsv2, hStackA⟩ :=
                                  sreprAll_cons_inv hStack
                                obtain ⟨wv1, ws0, rfl, hsv1, hStack'⟩ :=
                                  sreprAll_cons_inv hStackA
                                have hR2 : CanonRepr S x2 wv2 := hsv2
                                have hR1 : CanonRepr S x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : sub [wv1, wv2] with
                                | none => simp [wRunF, hHostFn, hpa, hb] at hrun
                                | some r =>
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals (r :: ws0)) := by
                                      simp [wRunF, hHostFn, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                      ih rest (node.id :: symRest) sLocals wLocals
                                        (.i (x1 - x2) :: svs) (r :: ws0) out hAdmR
                                        hTyR hLocalsTy
                                        (.cons (by simp [svalTy, hTyId]) hStackTy')
                                        hLocals
                                        (.cons (Ctr.hSub x1 x2 wv1 wv2 r hR1.1
                                          hR2.1 hb) hStack') hrun
                                    exact ⟨wl, ws, sStack', hout,
                                      by simp [sourceRunNodes, hk, popExpectedAll,
                                        popExpected, hsrc], hrel⟩
                | mul =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostFn : host f = some (2, mul) :=
                      hHost .mul f (by simp) hfEq
                    simp only [nodeTyped, hk] at hTyN
                    obtain ⟨hArgTy, hTyId⟩ := hTyN
                    cases args with
                    | nil => simp at hlen
                    | cons a1 t =>
                        cases t with
                        | nil => simp at hlen
                        | cons a2 t2 =>
                            cases t2 with
                            | cons a3 t3 =>
                                simp only [List.length_cons] at hlen; omega
                            | nil =>
                                simp at hsym
                                subst hsym
                                obtain ⟨sv2, svsA, rfl, hty2, hStackTyA⟩ :=
                                  idsTyped_cons_inv hStackTy
                                obtain ⟨sv1, svs, rfl, hty1, hStackTy'⟩ :=
                                  idsTyped_cons_inv hStackTyA
                                obtain ⟨x2, rfl⟩ := svalTy_int
                                  (hty2.trans (hArgTy a2 (by simp)))
                                obtain ⟨x1, rfl⟩ := svalTy_int
                                  (hty1.trans (hArgTy a1 (by simp)))
                                obtain ⟨wv2, wsA, rfl, hsv2, hStackA⟩ :=
                                  sreprAll_cons_inv hStack
                                obtain ⟨wv1, ws0, rfl, hsv1, hStack'⟩ :=
                                  sreprAll_cons_inv hStackA
                                have hR2 : CanonRepr S x2 wv2 := hsv2
                                have hR1 : CanonRepr S x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : mul [wv1, wv2] with
                                | none => simp [wRunF, hHostFn, hpa, hb] at hrun
                                | some r =>
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals (r :: ws0)) := by
                                      simp [wRunF, hHostFn, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                      ih rest (node.id :: symRest) sLocals wLocals
                                        (.i (x1 * x2) :: svs) (r :: ws0) out hAdmR
                                        hTyR hLocalsTy
                                        (.cons (by simp [svalTy, hTyId]) hStackTy')
                                        hLocals
                                        (.cons (Ctr.hMul x1 x2 wv1 wv2 r hR1.1
                                          hR2.1 hb) hStack') hrun
                                    exact ⟨wl, ws, sStack', hout,
                                      by simp [sourceRunNodes, hk, popExpectedAll,
                                        popExpected, hsrc], hrel⟩
                | cmp =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostFn : host f = some (2, cmp) :=
                      hHost .cmp f (by simp) hfEq
                    simp only [nodeTyped, hk] at hTyN
                    obtain ⟨hArgTy, hTyId⟩ := hTyN
                    cases args with
                    | nil => simp at hlen
                    | cons a1 t =>
                        cases t with
                        | nil => simp at hlen
                        | cons a2 t2 =>
                            cases t2 with
                            | cons a3 t3 =>
                                simp only [List.length_cons] at hlen; omega
                            | nil =>
                                simp at hsym
                                subst hsym
                                obtain ⟨sv2, svsA, rfl, hty2, hStackTyA⟩ :=
                                  idsTyped_cons_inv hStackTy
                                obtain ⟨sv1, svs, rfl, hty1, hStackTy'⟩ :=
                                  idsTyped_cons_inv hStackTyA
                                obtain ⟨x2, rfl⟩ := svalTy_int
                                  (hty2.trans (hArgTy a2 (by simp)))
                                obtain ⟨x1, rfl⟩ := svalTy_int
                                  (hty1.trans (hArgTy a1 (by simp)))
                                obtain ⟨wv2, wsA, rfl, hsv2, hStackA⟩ :=
                                  sreprAll_cons_inv hStack
                                obtain ⟨wv1, ws0, rfl, hsv1, hStack'⟩ :=
                                  sreprAll_cons_inv hStackA
                                have hR2 : CanonRepr S x2 wv2 := hsv2
                                have hR1 : CanonRepr S x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : cmp [wv1, wv2] with
                                | none => simp [wRunF, hHostFn, hpa, hb] at hrun
                                | some r =>
                                    have hr := Ctr.hCmp x1 x2 wv1 wv2 r hR1.1
                                      hR2.1 hR1.2 hR2.2 hb
                                    subst hr
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals
                                          (WVal.i32v (cmpW x1 x2) :: ws0)) := by
                                      simp [wRunF, hHostFn, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                      ih rest (node.id :: symRest) sLocals wLocals
                                        ((SVal.i32 (cmpW x1 x2)) :: svs)
                                        (WVal.i32v (cmpW x1 x2) :: ws0) out hAdmR
                                        hTyR hLocalsTy
                                        (.cons (by simp [svalTy, hTyId]) hStackTy')
                                        hLocals (.cons rfl hStack') hrun
                                    exact ⟨wl, ws, sStack', hout,
                                      by simp [sourceRunNodes, hk, popExpectedAll,
                                        popExpected, hsrc], hrel⟩
                | eq =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostFn : host f = some (2, eq) :=
                      hHost .eq f (by simp) hfEq
                    simp only [nodeTyped, hk] at hTyN
                    obtain ⟨hArgTy, hTyId⟩ := hTyN
                    cases args with
                    | nil => simp at hlen
                    | cons a1 t =>
                        cases t with
                        | nil => simp at hlen
                        | cons a2 t2 =>
                            cases t2 with
                            | cons a3 t3 =>
                                simp only [List.length_cons] at hlen; omega
                            | nil =>
                                simp at hsym
                                subst hsym
                                obtain ⟨sv2, svsA, rfl, hty2, hStackTyA⟩ :=
                                  idsTyped_cons_inv hStackTy
                                obtain ⟨sv1, svs, rfl, hty1, hStackTy'⟩ :=
                                  idsTyped_cons_inv hStackTyA
                                obtain ⟨x2, rfl⟩ := svalTy_int
                                  (hty2.trans (hArgTy a2 (by simp)))
                                obtain ⟨x1, rfl⟩ := svalTy_int
                                  (hty1.trans (hArgTy a1 (by simp)))
                                obtain ⟨wv2, wsA, rfl, hsv2, hStackA⟩ :=
                                  sreprAll_cons_inv hStack
                                obtain ⟨wv1, ws0, rfl, hsv1, hStack'⟩ :=
                                  sreprAll_cons_inv hStackA
                                have hR2 : CanonRepr S x2 wv2 := hsv2
                                have hR1 : CanonRepr S x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : eq [wv1, wv2] with
                                | none => simp [wRunF, hHostFn, hpa, hb] at hrun
                                | some r =>
                                    have hr := Ctr.hEq x1 x2 wv1 wv2 r hR1.1
                                      hR2.1 hR1.2 hR2.2 hb
                                    subst hr
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals
                                          (WVal.i32v (eqW x1 x2) :: ws0)) := by
                                      simp [wRunF, hHostFn, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                                      ih rest (node.id :: symRest) sLocals wLocals
                                        ((SVal.b (x1 = x2)) :: svs)
                                        (WVal.i32v (eqW x1 x2) :: ws0) out hAdmR
                                        hTyR hLocalsTy
                                        (.cons (by simp [svalTy, hTyId]) hStackTy')
                                        hLocals (.cons (eqW_b32 x1 x2) hStack') hrun
                                    exact ⟨wl, ws, sStack', hout,
                                      by simp [sourceRunNodes, hk, popExpectedAll,
                                        popExpected, hsrc], hrel⟩
          case selfCall tail f args =>
            simp [nodeAdmitted, hk] at hAdmN
          case ifElse cond thenBlock elseBlock =>
            simp [nodeAdmitted, hk] at hAdmN
          case vectorGetOrDefault arrTy toIndexIdx bIdx default =>
            simp [nodeAdmitted, hk] at hAdmN
          case structNew tyIdx args =>
            simp only [nodeTyped, hk] at hTyN
            obtain ⟨htiEq, hArgsInt, hTyId⟩ := hTyN
            subst tyIdx
            simp only [runNodesFuel, hk] at hrun
            cases hp : popExpectedAll symStack args.reverse with
            | none => simp [hp] at hrun
            | some symRest =>
                simp only [hp] at hrun
                have hsym := popExpectedAll_append hp
                subst hsym
                obtain ⟨svs1, svs2, rfl, hty1, hty2⟩ := idsTyped_split hStackTy
                obtain ⟨ws1, ws2, rfl, hrel1, hrel2⟩ := sreprAll_split hStack
                have hlen1 : svs1.length = args.length := by
                  simpa using idsTyped_length hty1
                have hlenw : ws1.length = args.length := by
                  rw [← sreprAll_length hrel1, hlen1]
                have hpa : popArgs args.length (ws1 ++ ws2)
                    = some (ws1.reverse, ws2) := by
                  rw [← hlenw]; exact popArgs_append
                have hwr : wRunF host ar callee
                    [.structNew structIdx args.length] wLocals (ws1 ++ ws2)
                    = some (.ok wLocals
                      (.structv structIdx ws1.reverse :: ws2)) := by
                  simp [wRunF, hpa]
                simp only [hwr] at hrun
                obtain ⟨fields, htake, hfra⟩ := takeInts_bridge hty1
                  (fun a ha => hArgsInt a (List.mem_reverse.mp ha)) hrel1 svs2
                rw [List.length_reverse] at htake
                obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                  ih rest (node.id :: symRest) sLocals wLocals
                    (.r fields :: svs2)
                    (.structv structIdx ws1.reverse :: ws2) out hAdmR hTyR
                    hLocalsTy (.cons (by simp [svalTy, hTyId]) hty2) hLocals
                    (.cons (srepr_rec hfra) hrel2) hrun
                exact ⟨wl, ws, sStack', hout,
                  by simp [sourceRunNodes, hk, hp, htake, hsrc], hrel⟩
          case intSignCmp op k scratch value =>
            simp only [nodeTyped, hk] at hTyN
            obtain ⟨hTyV, hScratch, hTyId⟩ := hTyN
            rw [hk] at hAdmN
            simp only [nodeAdmitted] at hAdmN
            simp only [runNodesFuel, hk] at hrun
            cases hp : popExpected symStack value with
            | none => simp [hp] at hrun
            | some symRest =>
                simp only [hp] at hrun
                have hsym := popExpected_eq hp
                subst hsym
                obtain ⟨sv, svs, rfl, hty1, hStackTy'⟩ :=
                  idsTyped_cons_inv hStackTy
                obtain ⟨n, rfl⟩ := svalTy_int (hty1.trans hTyV)
                obtain ⟨w, ws0, rfl, hsv, hStack'⟩ := sreprAll_cons_inv hStack
                have hsvC : CanonRepr S n w := hsv
                cases hstep : wRunF host ar callee
                    (intSignCmpTemplate C scratch op k) wLocals (w :: ws0) with
                | none => simp [hstep] at hrun
                | some out0 =>
                    have hout0 := intSignCmp_step S host ar callee op k scratch
                      wLocals ws0 n w out0 hAdmN hsvC.1 hsvC.2 hstep
                    subst hout0
                    simp only [hstep] at hrun
                    have hLocalsTy' : ∀ (i : Nat) (sv : SVal),
                        i < params.length →
                        (sLocals.set scratch (SVal.i n))[i]? = some sv →
                        params[i]? = some (svalTy sv) := by
                      intro i sv hi hset
                      rw [setNe? (SVal.i n) sLocals scratch i (by omega)] at hset
                      exact hLocalsTy i sv hi hset
                    obtain ⟨wl, ws, sStack', hout, hsrc, hrel⟩ :=
                      ih rest (node.id :: symRest) (sLocals.set scratch (.i n))
                        (wLocals.set scratch w)
                        (.b (symIntCmpDenote op n k) :: svs)
                        (b32 (symIntCmpDenote op n k) :: ws0) out hAdmR hTyR
                        hLocalsTy' (.cons (by simp [svalTy, hTyId]) hStackTy')
                        (sreprAll_set (sv := SVal.i n) hsvC hLocals scratch)
                        (.cons rfl hStack') hrun
                    exact ⟨wl, ws, sStack', hout,
                      by simp [sourceRunNodes, hk, popExpected, hsrc], hrel⟩

/-- Block-level corollary: a successful wasm run of an admitted body yields a
    single value SRepr-related to the source model's value. The wasm entry runs
    with the declared scratch pad appended to the arguments; the source model
    appends its `pad` twin, so the two locals lists are pointwise related from
    the start. -/
theorem sourceRunBlock_agrees
    {C : Nat} (S : CarrierSpec C) (structIdx : Nat)
    (box add sub mul cmp eq : List WVal → Option WVal)
    (Ctr : Contracts S box add sub mul cmp eq)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat))
    (hHost : ∀ role idx,
      role ∈ [HostRole.box, HostRole.add, HostRole.sub, HostRole.mul,
        HostRole.cmp, HostRole.eq] →
      AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx →
      host idx = some (roleArity role, roleFn box add sub mul cmp eq role))
    (fuel : Nat) (block : FragBlock)
    (sParams : List SVal) (wParams : List WVal) (out : Out)
    (hAdm : nodesAdmitted hostTable block.nodes = true)
    (tyOf : Nat → FragTy) (params : List FragTy)
    (hTy : planTyped structIdx tyOf params block.nodes)
    (hLenP : sParams.length = params.length)
    (hParamsTy : ∀ (i : Nat) (sv : SVal),
      sParams[i]? = some sv → params[i]? = some (svalTy sv))
    (hParams : SReprAll S structIdx sParams wParams)
    (hrun : runBlockFuel host ar callee fuel C block (wParams ++ [.null])
      = some out) :
    ∃ wLocals' w sv, out = .ok wLocals' [w] ∧
      sourceRunBlock fuel block sParams = some sv ∧
      SRepr S structIdx sv w := by
  cases fuel with
  | zero => simp [runBlockFuel] at hrun
  | succ fuel =>
      simp only [runBlockFuel] at hrun
      cases hr : runNodesFuel host ar callee fuel C block.nodes []
          (wParams ++ [WVal.null]) [] with
      | none => simp [hr] at hrun
      | some out0 =>
          rw [hr] at hrun
          have hLocalsTy : ∀ (i : Nat) (sv : SVal), i < params.length →
              (sParams ++ [SVal.pad])[i]? = some sv →
              params[i]? = some (svalTy sv) := by
            intro i sv hi hget
            have hlt : i < sParams.length := by omega
            rw [List.getElem?_append_left hlt] at hget
            exact hParamsTy i sv hget
          have hLocals : SReprAll S structIdx (sParams ++ [SVal.pad])
              (wParams ++ [WVal.null]) :=
            sreprAll_append hParams (.cons rfl .nil)
          obtain ⟨wl, ws, sStack', hout0, hsrc, hrepr⟩ :=
            agreement S structIdx box add sub mul cmp eq Ctr host ar callee
              hostTable hHost tyOf params fuel block.nodes []
              (sParams ++ [SVal.pad]) (wParams ++ [WVal.null]) [] [] out0
              hAdm hTy hLocalsTy IdsTyped.nil hLocals SReprAll.nil hr
          subst hout0
          cases ws with
          | nil => simp at hrun
          | cons v tail =>
              cases tail with
              | nil =>
                  cases hrepr with
                  | cons hv htail =>
                      cases htail
                      refine ⟨wl, v, _, ?_, ?_, hv⟩
                      · simpa using hrun.symm
                      · simp [sourceRunBlock, hsrc]
              | cons v' t' => simp at hrun


/-! ## Reverse completeness: lowered-code success implies plan-walker success

`ExprFragmentSoundness.mutualCorrect` gives planRun ⇒ instrRun; the discharge
also needs the converse for successful runs. -/

/-- One reverse step: a successful run of `[instr] ++ restInstrs` splits into
    the single-instruction step the plan walker takes and the continuation. -/
private theorem completeStepN
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (fuel carrier : Nat) (rest : List FragNode) (nextSym : List Nat)
    (instrs : List WInstr) (restInstrs : List WInstr)
    (locals stack : List WVal) (out : Out)
    (hrun : wRunF host ar callee (instrs ++ restInstrs) locals stack
      = some out)
    (hcont : ∀ l' s', wRunF host ar callee restInstrs l' s' = some out →
      runNodesFuel host ar callee fuel carrier rest nextSym l' s' = some out) :
    (match wRunF host ar callee instrs locals stack with
     | some (.ok locals' stack') =>
         runNodesFuel host ar callee fuel carrier rest nextSym locals' stack'
     | some (.ret value) => some (.ret value)
     | none => none) = some out := by
  rw [InterpreterSequencing.wRunF_append] at hrun
  cases hs : wRunF host ar callee instrs locals stack with
  | none => simp [InterpreterSequencing.seqOut, hs] at hrun
  | some stepOut =>
      cases stepOut with
      | ret v => simpa [InterpreterSequencing.seqOut, hs] using hrun
      | ok l' s' =>
          simp only [InterpreterSequencing.seqOut, hs] at hrun
          simp only [hs]
          exact hcont l' s' hrun

private theorem completeStep
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (fuel carrier : Nat) (rest : List FragNode) (nextSym : List Nat)
    (instr : WInstr) (restInstrs : List WInstr)
    (locals stack : List WVal) (out : Out)
    (hrun : wRunF host ar callee ([instr] ++ restInstrs) locals stack
      = some out)
    (hcont : ∀ l' s', wRunF host ar callee restInstrs l' s' = some out →
      runNodesFuel host ar callee fuel carrier rest nextSym l' s' = some out) :
    (match wRunF host ar callee [instr] locals stack with
     | some (.ok locals' stack') =>
         runNodesFuel host ar callee fuel carrier rest nextSym locals' stack'
     | some (.ret value) => some (.ret value)
     | none => none) = some out :=
  completeStepN host ar callee fuel carrier rest nextSym [instr] restInstrs
    locals stack out hrun hcont

/- SCOPE (documented choice): reverse completeness is stated over the SAME
   `nodesAdmitted` node set as `agreement`. The claim is FALSE for
   `vectorGetOrDefault`: its lowered template executes on the wasm side while
   the plan walker is deliberately fail-closed (`none`) on that node. An
   `ifElse` extension would additionally need the reverse frame discipline —
   restricting a successful branch run over the resting operand stack to the
   empty stack the plan walker starts branches on — i.e. a stack-depth
   invariant on lowered code that the v1 record-compute discharge does not
   need. The admitted straight-line kinds run the identical single instruction
   on both sides, so unlike the forward direction no `CallsOK` fence is
   needed: both runs consult the same host/ar/callee tables. -/
theorem runNodes_complete
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat)) :
    ∀ (fuel carrier : Nat) (nodes : List FragNode) (symStack : List Nat)
      (instrs : List WInstr) (finalStack : List Nat),
      nodesAdmitted hostTable nodes = true →
      lowerNodesFuel fuel carrier nodes symStack = some (instrs, finalStack) →
      ∀ (locals stack : List WVal) (out : Out),
        wRunF host ar callee instrs locals stack = some out →
        runNodesFuel host ar callee fuel carrier nodes symStack locals stack
          = some out := by
  intro fuel
  induction fuel with
  | zero =>
      intro carrier nodes symStack instrs finalStack _ hlow
      simp [lowerNodesFuel] at hlow
  | succ fuel ih =>
      intro carrier nodes symStack instrs finalStack hAdm hlow
        locals stack out hrun
      cases nodes with
      | nil =>
          simp only [lowerNodesFuel, Option.some.injEq, Prod.mk.injEq] at hlow
          obtain ⟨rfl, rfl⟩ := hlow
          simp only [wRunF, Option.some.injEq] at hrun
          subst hrun
          simp [runNodesFuel]
      | cons node rest =>
          have hAdmPair : nodeAdmitted hostTable node.kind = true ∧
              nodesAdmitted hostTable rest = true := by
            simpa [nodesAdmitted, List.all_cons] using hAdm
          obtain ⟨hAdmN, hAdmR⟩ := hAdmPair
          simp only [lowerNodesFuel] at hlow
          cases hk : node.kind
          case «local» index =>
            simp only [hk] at hlow
            cases hrest : lowerNodesFuel fuel carrier rest
                (node.id :: symStack) with
            | none => simp [hrest] at hlow
            | some pair =>
                obtain ⟨restInstrs, fin⟩ := pair
                simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                simp only [runNodesFuel, hk]
                exact completeStep host ar callee fuel carrier rest
                  (node.id :: symStack) (.localGet index) restInstrs
                  locals stack out hrun
                  (fun l' s' h => ih carrier rest (node.id :: symStack)
                    restInstrs fin hAdmR hrest l' s' out h)
          case constBool value =>
            simp [nodeAdmitted, hk] at hAdmN
          case constI64 value =>
            simp only [hk] at hlow
            cases hrest : lowerNodesFuel fuel carrier rest
                (node.id :: symStack) with
            | none => simp [hrest] at hlow
            | some pair =>
                obtain ⟨restInstrs, fin⟩ := pair
                simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                simp only [runNodesFuel, hk]
                exact completeStep host ar callee fuel carrier rest
                  (node.id :: symStack) (.i64Const value) restInstrs
                  locals stack out hrun
                  (fun l' s' h => ih carrier rest (node.id :: symStack)
                    restInstrs fin hAdmR hrest l' s' out h)
          case constI32 value =>
            simp only [hk] at hlow
            cases hrest : lowerNodesFuel fuel carrier rest
                (node.id :: symStack) with
            | none => simp [hrest] at hlow
            | some pair =>
                obtain ⟨restInstrs, fin⟩ := pair
                simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                obtain ⟨rfl, rfl⟩ := hlow
                simp only [runNodesFuel, hk]
                exact completeStep host ar callee fuel carrier rest
                  (node.id :: symStack) (.i32Const value) restInstrs
                  locals stack out hrun
                  (fun l' s' h => ih carrier rest (node.id :: symStack)
                    restInstrs fin hAdmR hrest l' s' out h)
          case constF64Bits bits =>
            simp [nodeAdmitted, hk] at hAdmN
          case structGet field receiver =>
            simp [nodeAdmitted, hk] at hAdmN
          case structGetUser tyIdx field value =>
            simp only [hk] at hlow
            cases hpop : popExpected symStack value with
            | none => simp [hpop] at hlow
            | some symRest =>
                simp only [hpop] at hlow
                cases hrest : lowerNodesFuel fuel carrier rest
                    (node.id :: symRest) with
                | none => simp [hrest] at hlow
                | some pair =>
                    obtain ⟨restInstrs, fin⟩ := pair
                    simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                    obtain ⟨rfl, rfl⟩ := hlow
                    simp only [runNodesFuel, hk, hpop]
                    exact completeStep host ar callee fuel carrier rest
                      (node.id :: symRest) (.structGet tyIdx field) restInstrs
                      locals stack out hrun
                      (fun l' s' h => ih carrier rest (node.id :: symRest)
                        restInstrs fin hAdmR hrest l' s' out h)
          case refIsNull value =>
            simp [nodeAdmitted, hk] at hAdmN
          case prim op args =>
            simp only [hk] at hlow
            cases hpop : popExpectedAll symStack args.reverse with
            | none => simp [hpop] at hlow
            | some symRest =>
                simp only [hpop] at hlow
                cases hrest : lowerNodesFuel fuel carrier rest
                    (node.id :: symRest) with
                | none => simp [hrest] at hlow
                | some pair =>
                    obtain ⟨restInstrs, fin⟩ := pair
                    simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                    obtain ⟨rfl, rfl⟩ := hlow
                    simp only [runNodesFuel, hk, hpop]
                    exact completeStep host ar callee fuel carrier rest
                      (node.id :: symRest) (primInstr op) restInstrs
                      locals stack out hrun
                      (fun l' s' h => ih carrier rest (node.id :: symRest)
                        restInstrs fin hAdmR hrest l' s' out h)
          case hostCall role funcIdx args =>
            simp only [hk] at hlow
            cases hpop : popExpectedAll symStack args.reverse with
            | none => simp [hpop] at hlow
            | some symRest =>
                simp only [hpop] at hlow
                cases hrest : lowerNodesFuel fuel carrier rest
                    (node.id :: symRest) with
                | none => simp [hrest] at hlow
                | some pair =>
                    obtain ⟨restInstrs, fin⟩ := pair
                    simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                    obtain ⟨rfl, rfl⟩ := hlow
                    simp only [runNodesFuel, hk, hpop]
                    exact completeStep host ar callee fuel carrier rest
                      (node.id :: symRest) (.call funcIdx) restInstrs
                      locals stack out hrun
                      (fun l' s' h => ih carrier rest (node.id :: symRest)
                        restInstrs fin hAdmR hrest l' s' out h)
          case selfCall tail funcIdx args =>
            simp [nodeAdmitted, hk] at hAdmN
          case ifElse cond thenBlock elseBlock =>
            simp [nodeAdmitted, hk] at hAdmN
          case vectorGetOrDefault arrTy toIndexIdx bIdx default =>
            simp [nodeAdmitted, hk] at hAdmN
          case structNew tyIdx args =>
            simp only [hk] at hlow
            cases hpop : popExpectedAll symStack args.reverse with
            | none => simp [hpop] at hlow
            | some symRest =>
                simp only [hpop] at hlow
                cases hrest : lowerNodesFuel fuel carrier rest
                    (node.id :: symRest) with
                | none => simp [hrest] at hlow
                | some pair =>
                    obtain ⟨restInstrs, fin⟩ := pair
                    simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                    obtain ⟨rfl, rfl⟩ := hlow
                    simp only [runNodesFuel, hk, hpop]
                    exact completeStep host ar callee fuel carrier rest
                      (node.id :: symRest) (.structNew tyIdx args.length)
                      restInstrs locals stack out hrun
                      (fun l' s' h => ih carrier rest (node.id :: symRest)
                        restInstrs fin hAdmR hrest l' s' out h)
          case intSignCmp op k scratch value =>
            simp only [hk] at hlow
            cases hpop : popExpected symStack value with
            | none => simp [hpop] at hlow
            | some symRest =>
                simp only [hpop] at hlow
                cases hrest : lowerNodesFuel fuel carrier rest
                    (node.id :: symRest) with
                | none => simp [hrest] at hlow
                | some pair =>
                    obtain ⟨restInstrs, fin⟩ := pair
                    simp only [hrest, Option.some.injEq, Prod.mk.injEq] at hlow
                    obtain ⟨rfl, rfl⟩ := hlow
                    simp only [runNodesFuel, hk, hpop]
                    exact completeStepN host ar callee fuel carrier rest
                      (node.id :: symRest)
                      (intSignCmpTemplate carrier scratch op k) restInstrs
                      locals stack out hrun
                      (fun l' s' h => ih carrier rest (node.id :: symRest)
                        restInstrs fin hAdmR hrest l' s' out h)

/-- Block-level corollary. The `hshape` hypothesis is the single-value (or
    early-return) result shape that block lowering guarantees and that the
    discharge possesses concretely; demanding it here avoids re-deriving the
    stack-arity invariant of lowered code from the run itself. -/
theorem runBlock_complete
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat))
    (fuel carrier : Nat) (block : FragBlock) (instrs : List WInstr)
    (locals : List WVal) (out : Out)
    (hAdm : nodesAdmitted hostTable block.nodes = true)
    (hlow : lowerBlockFuel fuel carrier block = some instrs)
    (hrun : wRunF host ar callee instrs locals [] = some out)
    (hshape : (∃ ls v, out = .ok ls [v]) ∨ (∃ v, out = .ret v)) :
    runBlockFuel host ar callee fuel carrier block locals = some out := by
  cases fuel with
  | zero => simp [lowerBlockFuel] at hlow
  | succ fuel =>
      simp only [lowerBlockFuel] at hlow
      cases hn : lowerNodesFuel fuel carrier block.nodes [] with
      | none => simp [hn] at hlow
      | some pair =>
          obtain ⟨is, fs⟩ := pair
          rw [hn] at hlow
          cases fs with
          | nil => simp at hlow
          | cons r rs =>
              cases rs with
              | cons r' rs' => simp at hlow
              | nil =>
                  by_cases hr : r = block.result
                  · subst hr
                    have his : is = instrs := by simpa using hlow
                    subst his
                    have hcomp := runNodes_complete host ar callee
                      hostTable fuel carrier
                      block.nodes [] is [block.result] hAdm hn locals [] out hrun
                    simp only [runBlockFuel, hcomp]
                    obtain ⟨ls, v, rfl⟩ | ⟨v, rfl⟩ := hshape <;> simp
                  · simp [hr] at hlow

/-! ## Executable typing twin

`planTypedB` is the Bool face the admission-time recognizer evaluates; the
soundness lemma converts its acceptance into the `planTyped` hypothesis the
`agreement` theorem consumes. Every clause mirrors `nodeTyped` conjunct for
conjunct, so acceptance cannot fail open. -/

def nodeTypedB (structIdx : Nat) (tyOf : Nat → FragTy)
    (params : List FragTy) (node : FragNode) : Bool :=
  match node.kind with
  | .local index => params[index]? == some (tyOf node.id)
  | .constI64 _ => tyOf node.id == .i64
  | .constI32 _ => tyOf node.id == .rawI32
  | .structGetUser _ _ value =>
      tyOf value == .adtRef && tyOf node.id == .intCarrier
  | .structNew tyIdx args =>
      tyIdx == structIdx &&
        (args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .adtRef)
  | .prim _ args =>
      args.all (fun a => tyOf a == .rawI32) && tyOf node.id == .boolI32
  | .hostCall .box _ args =>
      args.all (fun a => tyOf a == .i64) && tyOf node.id == .intCarrier
  | .hostCall .cmp _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .rawI32
  | .hostCall .eq _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .boolI32
  | .hostCall _ _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .intCarrier
  | .intSignCmp _ _ scratch value =>
      tyOf value == .intCarrier && scratch == params.length &&
        tyOf node.id == .boolI32
  | _ => true

def planTypedB (structIdx : Nat) (tyOf : Nat → FragTy)
    (params : List FragTy) (nodes : List FragNode) : Bool :=
  nodes.all (nodeTypedB structIdx tyOf params)

theorem nodeTypedB_sound {structIdx : Nat} {tyOf : Nat → FragTy}
    {params : List FragTy} {node : FragNode}
    (h : nodeTypedB structIdx tyOf params node = true) :
    nodeTyped structIdx tyOf params node := by
  cases hk : node.kind
  case hostCall role funcIdx args =>
    cases role <;>
      simp only [nodeTypedB, nodeTyped, hk, Bool.and_eq_true, beq_iff_eq,
        List.all_eq_true] at h ⊢ <;>
      exact h
  case intSignCmp op constant scratch value =>
    simp only [nodeTypedB, nodeTyped, hk, Bool.and_eq_true, beq_iff_eq] at h ⊢
    exact ⟨h.1.1, h.1.2, h.2⟩
  all_goals
    simp only [nodeTypedB, nodeTyped, hk, Bool.and_eq_true, beq_iff_eq,
      List.all_eq_true] at h ⊢ <;>
    try exact h

theorem planTypedB_sound {structIdx : Nat} {tyOf : Nat → FragTy}
    {params : List FragTy} {nodes : List FragNode}
    (h : planTypedB structIdx tyOf params nodes = true) :
    planTyped structIdx tyOf params nodes := by
  induction nodes with
  | nil => exact True.intro
  | cons node rest ih =>
      simp only [planTypedB, List.all_cons, Bool.and_eq_true] at h
      exact ⟨nodeTypedB_sound h.1, ih h.2⟩

end RecordComputeBridge
