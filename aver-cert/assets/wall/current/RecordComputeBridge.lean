/- G-a Phase 1: the generic source-eval bridge for the record projection-compute
   face, v1 node set (what the k5 gate needs):

     local / constI64 / structGetUser / structNew / hostCall {box, add, sub, mul}

   over ONE user struct type whose fields are all Int carriers (Fraction).

   `sourceRunNodes` mirrors `ExprFragmentSemantics.runNodesFuel` ARM BY ARM —
   same fuel discipline, same symbolic-id stack, same popExpected/popExpectedAll
   plumbing — but computes over SOURCE values (ℤ, Bool, raw i64 literals,
   records-as-Int-lists). The agreement theorem walks both evaluators in
   lockstep: pointwise-SRepr stacks stay related at every step (locals are
   never written by this node set), host calls are bridged by the named
   box/add/sub/mul contracts, and `structNew`/`structGetUser` are bridged by
   the record representation. The obligation's model for the generic face is
   `sourceRunBlock` — the plan IS the claim. -/
import ExprFragmentSoundness

open CertPrelude AverCert.Schema AverCert.PlanLower ExprFragmentSemantics

namespace RecordComputeBridge

/-- Source-level values for the v1 face. `raw` is a bare i64 literal on its
    way into the `box` helper (the emitter's `i64.const k; call box` idiom) —
    distinct from `i`, which is a boxed source integer in the carrier. -/
inductive SVal where
  | i (n : Int)
  | b (v : Bool)
  | raw (n : Int)
  | r (fields : List Int)
deriving Repr

/-- Representation of one source value by one wasm value, over carrier
    representation `Repr` and the single user struct type `structIdx`. -/
def SRepr (Repr : Int → WVal → Prop) (structIdx : Nat) : SVal → WVal → Prop
  | .i n, w => Repr n w
  | .b v, w => w = b32 v
  | .raw n, w => w = .i64v n
  | .r fields, w =>
      ∃ ws, w = .structv structIdx ws ∧ ReprAll Repr fields ws

/-- Pointwise representation of a source stack / locals list. -/
inductive SReprAll (Repr : Int → WVal → Prop) (structIdx : Nat) :
    List SVal → List WVal → Prop where
  | nil : SReprAll Repr structIdx [] []
  | cons {sv w ss ws} : SRepr Repr structIdx sv w →
      SReprAll Repr structIdx ss ws →
      SReprAll Repr structIdx (sv :: ss) (w :: ws)

/-- Named host contracts of the v1 face. add/sub/mul are exactly the
    hypotheses `Obligation.holds` threads; `box` is the boxing helper's
    meaning (its body is byte-pinned, so at face level this is the synthesized
    semantics, not a new trust assumption). `eq` is NOT part of the v1 set:
    the wall's `_hEq` obligation contract is small-band, so the strong
    bridge-level contract cannot be met — the face never admits `eq` calls. -/
structure Contracts (Repr : Int → WVal → Prop)
    (box add sub mul : List WVal → Option WVal) : Prop where
  hBox : ∀ n w, box [.i64v n] = some w → Repr n w
  hAdd : ∀ a b va vb w, Repr a va → Repr b vb →
    add [va, vb] = some w → Repr (a + b) w
  hSub : ∀ a b va vb w, Repr a va → Repr b vb →
    sub [va, vb] = some w → Repr (a - b) w
  hMul : ∀ a b va vb w, Repr a va → Repr b vb →
    mul [va, vb] = some w → Repr (a * b) w

/-- The wasm arity of each host role's signature. -/
def roleArity : HostRole → Nat
  | .box => 1
  | .toIndex => 1
  | _ => 2

/-- The contract function a used role denotes; the roles the v1 face never
    admits map to the trap-only function. -/
def roleFn (box add sub mul : List WVal → Option WVal) :
    HostRole → List WVal → Option WVal
  | .box => box
  | .add => add
  | .sub => sub
  | .mul => mul
  | _ => fun _ => none

/-- Which node kinds the v1 face admits, keyed on the byte-derived role
    TABLE: a host call is admitted only when the table resolves its role to
    exactly the cited function index. A role the table lacks fail-closes.
    Everything else is fail-closed. -/
def nodeAdmitted (hostTable : List (HostRole × Nat)) :
    FragNodeKind → Bool
  | .local _ => true
  | .constI64 _ => true
  | .structGetUser _ _ _ => true
  | .structNew _ _ => true
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
              | _, _ => none
          | none => none
      | _ => none

/-- The v1 face's model: run the plan body over source inputs. Fuel is
    peeled exactly as `runBlockFuel` peels it, so the two block evaluators are
    twins level by level. -/
def sourceRunBlock : Nat → FragBlock → List SVal → Option SVal
  | 0, _, _ => none
  | fuel + 1, block, params =>
      match sourceRunNodes fuel block.nodes [] params [] with
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
  | .raw _ => .i64
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
  | .structGetUser _ _ value =>
      tyOf value = .adtRef ∧ tyOf node.id = .intCarrier
  | .structNew tyIdx args =>
      tyIdx = structIdx ∧ (∀ a ∈ args, tyOf a = .intCarrier) ∧
        tyOf node.id = .adtRef
  | .hostCall .box _ args =>
      (∀ a ∈ args, tyOf a = .i64) ∧ tyOf node.id = .intCarrier
  | .hostCall _ _ args =>
      (∀ a ∈ args, tyOf a = .intCarrier) ∧ tyOf node.id = .intCarrier
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

private theorem sreprAll_cons_inv {Repr : Int → WVal → Prop} {structIdx : Nat}
    {sv : SVal} {svs : List SVal} {ws : List WVal}
    (h : SReprAll Repr structIdx (sv :: svs) ws) :
    ∃ w ws', ws = w :: ws' ∧ SRepr Repr structIdx sv w ∧
      SReprAll Repr structIdx svs ws' := by
  cases h with
  | cons h1 h2 => exact ⟨_, _, rfl, h1, h2⟩

private theorem sreprAll_length {Repr : Int → WVal → Prop} {structIdx : Nat}
    {ss : List SVal} {ws : List WVal}
    (h : SReprAll Repr structIdx ss ws) : ss.length = ws.length := by
  induction h with
  | nil => rfl
  | cons _ _ ih => simp [ih]

private theorem sreprAll_getElem? {Repr : Int → WVal → Prop} {structIdx : Nat}
    {ss : List SVal} {ws : List WVal}
    (h : SReprAll Repr structIdx ss ws) {i : Nat} {wv : WVal}
    (hw : ws[i]? = some wv) :
    ∃ sv, ss[i]? = some sv ∧ SRepr Repr structIdx sv wv := by
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

private theorem sreprAll_split {Repr : Int → WVal → Prop} {structIdx : Nat}
    {ss1 : List SVal} :
    ∀ {ss2 : List SVal} {ws : List WVal},
      SReprAll Repr structIdx (ss1 ++ ss2) ws →
      ∃ ws1 ws2, ws = ws1 ++ ws2 ∧ SReprAll Repr structIdx ss1 ws1 ∧
        SReprAll Repr structIdx ss2 ws2 := by
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

private theorem srepr_rec {Repr : Int → WVal → Prop} {structIdx : Nat}
    {fields : List Int} {ws : List WVal} (h : ReprAll Repr fields ws) :
    SRepr Repr structIdx (.r fields) (.structv structIdx ws) :=
  ⟨ws, rfl, h⟩

private theorem takeInts_bridge {Repr : Int → WVal → Prop} {structIdx : Nat}
    {tyOf : Nat → FragTy} {ids : List Nat} {svs : List SVal}
    (hty : IdsTyped tyOf ids svs) :
    (∀ a ∈ ids, tyOf a = .intCarrier) →
    ∀ {ws : List WVal}, SReprAll Repr structIdx svs ws →
      ∀ sRest : List SVal,
        ∃ fields, takeInts ids.length (svs ++ sRest) = some (fields, sRest) ∧
          ReprAll Repr fields ws.reverse := by
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
      have hR : Repr n w := h1
      obtain ⟨fields, htake, hra⟩ :=
        ih (fun a ha => hint a (List.mem_cons_of_mem _ ha)) h2 sRest
      refine ⟨fields ++ [n], ?_, ?_⟩
      · simp only [List.length_cons, List.cons_append, takeInts, htake]
      · simpa [List.reverse_cons] using reprAll_append_single hra hR

/-! ## Agreement

The wasm-side evaluator and the source-side evaluator stay pointwise-SRepr
related at every step. The admitted node set never writes locals and never
returns early, so the conclusion speaks only about the final stack. -/

/- STATEMENT ADJUSTMENT (authorized, documented): as originally stated the
   theorem is unprovable for ill-typed plans — wasm `struct.new` packs ANY
   operand values and an abstract host function may succeed on non-carrier
   arguments, while the source evaluator demands boxed integers there, so the
   source run can fail where the wasm run succeeds (e.g. a `constI64` result
   fed straight into `structNew`, or a record fed to `add`). The minimal
   repair threads the plan's type discipline, which `PlanCheck.checkBlockFuel`
   enforces on every accepted plan: added hypotheses `hTy` (each admitted node
   consumes/produces its declared `FragTy`, pinning `structNew`'s type index
   to the ONE user struct type — nothing on the wasm side forces that pin),
   `hLocalsTy` (locals inhabit `params`), and `hStackTy` (the symbolic stack
   stays pointwise typed). `structGetUser`'s type index needs no pin: the wasm
   struct-tag check forces `tyIdx = structIdx` on any successful run. -/
theorem agreement
    (Repr : Int → WVal → Prop) (structIdx : Nat)
    (box add sub mul : List WVal → Option WVal)
    (C : Contracts Repr box add sub mul)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (hHost : ∀ role idx,
      role ∈ [HostRole.box, HostRole.add, HostRole.sub, HostRole.mul] →
      AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx →
      host idx = some (roleArity role, roleFn box add sub mul role))
    (fuel : Nat)
    (nodes : List FragNode) (symStack : List Nat)
    (sLocals : List SVal) (wLocals : List WVal)
    (sStack : List SVal) (wStack : List WVal)
    (wLocals' wStack' : List WVal)
    (hAdm : nodesAdmitted hostTable nodes = true)
    (tyOf : Nat → FragTy) (params : List FragTy)
    (hTy : planTyped structIdx tyOf params nodes)
    (hLocalsTy : ∀ (i : Nat) (sv : SVal),
      sLocals[i]? = some sv → params[i]? = some (svalTy sv))
    (hStackTy : IdsTyped tyOf symStack sStack)
    (hLocals : SReprAll Repr structIdx sLocals wLocals)
    (hStack : SReprAll Repr structIdx sStack wStack)
    (hrun : runNodesFuel host ar callee fuel carrier nodes symStack
      wLocals wStack = some (.ok wLocals' wStack')) :
    ∃ sStack',
      sourceRunNodes fuel nodes symStack sLocals sStack = some sStack' ∧
      SReprAll Repr structIdx sStack' wStack' := by
  induction fuel generalizing nodes symStack sStack wStack wLocals' wStack' with
  | zero => simp [runNodesFuel] at hrun
  | succ fuel ih =>
      cases nodes with
      | nil =>
          simp only [runNodesFuel, Option.some.injEq, Out.ok.injEq] at hrun
          exact ⟨sStack, by simp [sourceRunNodes], hrun.2 ▸ hStack⟩
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
                have hty : svalTy sv = tyOf node.id :=
                  Option.some.inj ((hLocalsTy index sv hsl).symm.trans hTyN)
                obtain ⟨sStack', hsrc, hrel⟩ := ih rest (node.id :: symStack)
                  (sv :: sStack) (wv :: wStack) wLocals' wStack' hAdmR hTyR
                  (.cons hty hStackTy) (.cons hsv hStack) hrun
                refine ⟨sStack', ?_, hrel⟩
                simp [sourceRunNodes, hk, hsl, hsrc]
          case constBool value =>
            simp [nodeAdmitted, hk] at hAdmN
          case constI64 value =>
            simp only [nodeTyped, hk] at hTyN
            simp only [runNodesFuel, hk] at hrun
            have hwr : wRunF host ar callee [.i64Const value] wLocals wStack
                = some (.ok wLocals (.i64v value :: wStack)) := by
              simp [wRunF]
            simp only [hwr] at hrun
            obtain ⟨sStack', hsrc, hrel⟩ := ih rest (node.id :: symStack)
              (.raw value :: sStack) (.i64v value :: wStack) wLocals' wStack'
              hAdmR hTyR (.cons (by simp [svalTy, hTyN]) hStackTy)
              (.cons rfl hStack) hrun
            refine ⟨sStack', ?_, hrel⟩
            simp [sourceRunNodes, hk, hsrc]
          case constI32 value =>
            simp [nodeAdmitted, hk] at hAdmN
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
                obtain ⟨sv, svs, rfl, hty1, hStackTy'⟩ := idsTyped_cons_inv hStackTy
                obtain ⟨fields, rfl⟩ := svalTy_adt (hty1.trans hTyV)
                obtain ⟨w, ws0, rfl, hsv, hStack'⟩ := sreprAll_cons_inv hStack
                have hsv' : ∃ wsf, w = .structv structIdx wsf ∧
                    ReprAll Repr fields wsf := hsv
                obtain ⟨wsf, rfl, hra⟩ := hsv'
                by_cases hti : structIdx = tyIdx
                · subst hti
                  cases hf : wsf[field]? with
                  | none => simp [wRunF, hf] at hrun
                  | some wv =>
                      have hwr : wRunF host ar callee [.structGet structIdx field]
                          wLocals (.structv structIdx wsf :: ws0)
                          = some (.ok wLocals (wv :: ws0)) := by
                        simp [wRunF, hf]
                      simp only [hwr] at hrun
                      obtain ⟨m, hfm, hRm⟩ := reprAll_getElem? hra hf
                      obtain ⟨sStack', hsrc, hrel⟩ := ih rest (node.id :: symRest)
                        (.i m :: svs) (wv :: ws0) wLocals' wStack' hAdmR hTyR
                        (.cons (by simp [svalTy, hTyId]) hStackTy')
                        (.cons hRm hStack') hrun
                      refine ⟨sStack', ?_, hrel⟩
                      simp [sourceRunNodes, hk, popExpected, hfm, hsrc]
                · simp [wRunF, hti] at hrun
          case refIsNull value =>
            simp [nodeAdmitted, hk] at hAdmN
          case prim op args =>
            simp [nodeAdmitted, hk] at hAdmN
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
                | cmp => simp [nodeAdmitted] at hAdmN
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
                            have hw : w = .i64v n := hsv
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
                                obtain ⟨sStack', hsrc, hrel⟩ := ih rest
                                  (node.id :: symRest) (.i n :: svs) (r :: ws0)
                                  wLocals' wStack' hAdmR hTyR
                                  (.cons (by simp [svalTy, hTyId]) hStackTy')
                                  (.cons (C.hBox n r hb) hStack') hrun
                                refine ⟨sStack', ?_, hrel⟩
                                simp [sourceRunNodes, hk, popExpectedAll,
                                  popExpected, hsrc]
                | add =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostAdd : host f = some (2, add) :=
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
                                have hR2 : Repr x2 wv2 := hsv2
                                have hR1 : Repr x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : add [wv1, wv2] with
                                | none => simp [wRunF, hHostAdd, hpa, hb] at hrun
                                | some r =>
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals (r :: ws0)) := by
                                      simp [wRunF, hHostAdd, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨sStack', hsrc, hrel⟩ := ih rest
                                      (node.id :: symRest) (.i (x1 + x2) :: svs)
                                      (r :: ws0) wLocals' wStack' hAdmR hTyR
                                      (.cons (by simp [svalTy, hTyId]) hStackTy')
                                      (.cons (C.hAdd x1 x2 wv1 wv2 r hR1 hR2 hb)
                                        hStack') hrun
                                    refine ⟨sStack', ?_, hrel⟩
                                    simp [sourceRunNodes, hk, popExpectedAll,
                                      popExpected, hsrc]
                | sub =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostSub : host f = some (2, sub) :=
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
                                have hR2 : Repr x2 wv2 := hsv2
                                have hR1 : Repr x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : sub [wv1, wv2] with
                                | none => simp [wRunF, hHostSub, hpa, hb] at hrun
                                | some r =>
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals (r :: ws0)) := by
                                      simp [wRunF, hHostSub, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨sStack', hsrc, hrel⟩ := ih rest
                                      (node.id :: symRest) (.i (x1 - x2) :: svs)
                                      (r :: ws0) wLocals' wStack' hAdmR hTyR
                                      (.cons (by simp [svalTy, hTyId]) hStackTy')
                                      (.cons (C.hSub x1 x2 wv1 wv2 r hR1 hR2 hb)
                                        hStack') hrun
                                    refine ⟨sStack', ?_, hrel⟩
                                    simp [sourceRunNodes, hk, popExpectedAll,
                                      popExpected, hsrc]
                | mul =>
                    simp only [nodeAdmitted, Bool.and_eq_true,
                      beq_iff_eq] at hAdmN
                    obtain ⟨hfEq, hlen⟩ := hAdmN
                    have hHostMul : host f = some (2, mul) :=
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
                                have hR2 : Repr x2 wv2 := hsv2
                                have hR1 : Repr x1 wv1 := hsv1
                                have hpa : popArgs 2 (wv2 :: wv1 :: ws0)
                                    = some ([wv1, wv2], ws0) := by
                                  simpa using popArgs_append
                                    (ws1 := [wv2, wv1]) (wRest := ws0)
                                cases hb : mul [wv1, wv2] with
                                | none => simp [wRunF, hHostMul, hpa, hb] at hrun
                                | some r =>
                                    have hwr : wRunF host ar callee [.call f]
                                        wLocals (wv2 :: wv1 :: ws0)
                                        = some (.ok wLocals (r :: ws0)) := by
                                      simp [wRunF, hHostMul, hpa, hb]
                                    simp only [hwr] at hrun
                                    obtain ⟨sStack', hsrc, hrel⟩ := ih rest
                                      (node.id :: symRest) (.i (x1 * x2) :: svs)
                                      (r :: ws0) wLocals' wStack' hAdmR hTyR
                                      (.cons (by simp [svalTy, hTyId]) hStackTy')
                                      (.cons (C.hMul x1 x2 wv1 wv2 r hR1 hR2 hb)
                                        hStack') hrun
                                    refine ⟨sStack', ?_, hrel⟩
                                    simp [sourceRunNodes, hk, popExpectedAll,
                                      popExpected, hsrc]
                | eq => simp [nodeAdmitted] at hAdmN
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
                have hwr : wRunF host ar callee [.structNew structIdx args.length]
                    wLocals (ws1 ++ ws2)
                    = some (.ok wLocals (.structv structIdx ws1.reverse :: ws2)) := by
                  simp [wRunF, hpa]
                simp only [hwr] at hrun
                obtain ⟨fields, htake, hfra⟩ := takeInts_bridge hty1
                  (fun a ha => hArgsInt a (List.mem_reverse.mp ha)) hrel1 svs2
                rw [List.length_reverse] at htake
                obtain ⟨sStack', hsrc, hrel⟩ := ih rest (node.id :: symRest)
                  (.r fields :: svs2) (.structv structIdx ws1.reverse :: ws2)
                  wLocals' wStack' hAdmR hTyR
                  (.cons (by simp [svalTy, hTyId]) hty2)
                  (.cons (srepr_rec hfra) hrel2) hrun
                refine ⟨sStack', ?_, hrel⟩
                simp [sourceRunNodes, hk, hp, htake, hsrc]

/-- Block-level corollary: a successful wasm run of an admitted body yields a
    value SRepr-related to the source model's value. -/
theorem sourceRunBlock_agrees
    (Repr : Int → WVal → Prop) (structIdx : Nat)
    (box add sub mul : List WVal → Option WVal)
    (C : Contracts Repr box add sub mul)
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (hHost : ∀ role idx,
      role ∈ [HostRole.box, HostRole.add, HostRole.sub, HostRole.mul] →
      AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx →
      host idx = some (roleArity role, roleFn box add sub mul role))
    (fuel : Nat) (block : FragBlock)
    (sParams : List SVal) (wParams : List WVal)
    (wLocals' : List WVal) (w : WVal)
    (hAdm : nodesAdmitted hostTable block.nodes = true)
    (tyOf : Nat → FragTy) (params : List FragTy)
    (hTy : planTyped structIdx tyOf params block.nodes)
    (hParamsTy : ∀ (i : Nat) (sv : SVal),
      sParams[i]? = some sv → params[i]? = some (svalTy sv))
    (hParams : SReprAll Repr structIdx sParams wParams)
    (hrun : runBlockFuel host ar callee fuel carrier block wParams =
      some (.ok wLocals' [w])) :
    ∃ sv, sourceRunBlock fuel block sParams = some sv ∧
      SRepr Repr structIdx sv w := by
  cases fuel with
  | zero => simp [runBlockFuel] at hrun
  | succ fuel =>
      simp only [runBlockFuel] at hrun
      cases hr : runNodesFuel host ar callee fuel carrier block.nodes []
          wParams [] with
      | none => simp [hr] at hrun
      | some out =>
          rw [hr] at hrun
          cases out with
          | ret value => simp at hrun
          | ok ls st =>
              match st, hrun with
              | [value], hrun =>
                  have hs : ls = wLocals' ∧ value = w := by simpa using hrun
                  obtain ⟨rfl, rfl⟩ := hs
                  obtain ⟨sStack', hsrc, hrepr⟩ := agreement Repr structIdx
                    box add sub mul C host ar callee carrier
                    hostTable hHost
                    fuel block.nodes [] sParams wParams [] [] ls [value]
                    hAdm tyOf params hTy hParamsTy IdsTyped.nil hParams
                    (SReprAll.nil) hr
                  cases hrepr with
                  | cons hv htail =>
                      cases htail
                      exact ⟨_, by simp [sourceRunBlock, hsrc], hv⟩

/-! ## Reverse completeness: lowered-code success implies plan-walker success

`ExprFragmentSoundness.mutualCorrect` gives planRun ⇒ instrRun; the discharge
also needs the converse for successful runs. -/

/-- One reverse step: a successful run of `[instr] ++ restInstrs` splits into
    the single-instruction step the plan walker takes and the continuation. -/
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
     | none => none) = some out := by
  rw [InterpreterSequencing.wRunF_append] at hrun
  cases hs : wRunF host ar callee [instr] locals stack with
  | none => simp [InterpreterSequencing.seqOut, hs] at hrun
  | some stepOut =>
      cases stepOut with
      | ret v => simpa [InterpreterSequencing.seqOut, hs] using hrun
      | ok l' s' =>
          simp only [InterpreterSequencing.seqOut, hs] at hrun
          simp only [hs]
          exact hcont l' s' hrun

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
            simp [nodeAdmitted, hk] at hAdmN
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
            simp [nodeAdmitted, hk] at hAdmN
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
  | .structGetUser _ _ value =>
      tyOf value == .adtRef && tyOf node.id == .intCarrier
  | .structNew tyIdx args =>
      tyIdx == structIdx &&
        (args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .adtRef)
  | .hostCall .box _ args =>
      args.all (fun a => tyOf a == .i64) && tyOf node.id == .intCarrier
  | .hostCall _ _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .intCarrier
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
