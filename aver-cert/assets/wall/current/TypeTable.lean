/- TypeTable — the declared module layout, confirmed against the bytes.

   A certificate DECLARES, for every source type its plans mention, the wasm
   type index that represents it (`Schema.TypeTable`), and the runtime helper
   indices (`Schema.Subject`). This file turns those declarations into the
   lowering context `Grammar.MCtx` (`mctxOf`), and confirms each declared entry
   against the module's type and data sections:

   * the Int carrier is the one `CertDecode.carrierState` finds, or absent
     exactly when the module has none; its limb field names the declared
     magnitude array;
   * every struct and array the table names is an entry of the rec group that
     OPENS the type section (so it starts at type index 0), and the entry's
     field storage is exactly the representation of the declared field types:
     records and tuples, constructor structs `sub final root`, sum roots
     `sub (struct)`, Option `{i32, T}`, Result `{i32, T, E}`, `List<T>`
     `{T, ref null self}`, `Vector<T>` arrays, `$string` = `(array (mut i8))`
     and `Vector<String>` = `(array (ref null $string))`;
   * a one-field (newtype) record is represented by its field's value, whose
     declared heap type must be the record's declared index (a raw-`i64`
     newtype has no heap type and is declined);
   * every sum passes `Grammar.sumOk` (distinct constructor structs) and the
     S-3 pin `GrammarLower.S3Pin` over the raw bytes of the opening rec group;
   * no struct index serves two declarations;
   * every string literal's data segment holds exactly its bytes
     (`GrammarLower.DataPin`, S-11).

   An index the table or the subject does not declare lowers to `absent k`,
   which is outside the u32 index space: any lowering that writes it fails to
   encode, so a plan citing an undeclared type or helper declines. -/
import GrammarLower
import SchemaCore
import CertDecode

namespace AverCert.TypeTable
open CertPrelude AverCert.Schema AverCert.Grammar

/-! ## The lowering context of a manifest -/

/-- An index that is never a u32: every encoder of the lowering rejects it. -/
def absent (k : Nat) : Nat := 4294967296 + k

def idxOr (k : Nat) : Option Nat → Nat
  | some i => i
  | none => absent k

def lookupTy (k : Nat) (xs : List (Ty × Nat)) (t : Ty) : Nat :=
  ((xs.find? fun x => decide (x.1 = t)).map (·.2)).getD (absent k)

def lookupNat (k : Nat) (xs : List (Nat × Nat)) (t : Nat) : Nat :=
  ((xs.find? fun x => x.1 == t).map (·.2)).getD (absent k)

def recordOf (tt : TypeTable) (tid : Nat) : Option RecordDecl :=
  tt.records.find? (·.tid == tid)

def sumOf (tt : TypeTable) (tid : Nat) : Option SumDecl :=
  tt.sums.find? (·.tid == tid)

def entryOf (fns : List FnEntry) (f : Nat) : Option FnEntry :=
  fns.find? (·.funcIdx == f)

/-- The planned function at a function index (first binding). -/
def planOf (fns : List FnEntry) (f : Nat) : Option FnPlan :=
  (entryOf fns f).map (·.plan)

def roleOf (roles : Option CertDecode.AddSub.Roles) (pick : CertDecode.AddSub.Roles → Option Nat) :
    Option Nat :=
  roles.bind pick

def stringRole (rs : List (Nat × CertDecode.StringHost.Role)) (r : CertDecode.StringHost.Role) :
    Option Nat :=
  (rs.find? fun x => x.2 == r).map (·.1)

/-- The lowering context of a manifest: every index from a declaration, and
    `absent` where there is none. The Int negation helper is not a declared
    role (its body has no wall template yet), so a plan with `neg` lowers to
    `call (absent 5)` and declines. -/
def mctxOf (s : Subject) (tt : TypeTable) (fns : List FnEntry) : MCtx :=
  { carrier := idxOr 0 tt.carrier
    box := idxOr 1 (roleOf s.hostRoleTable (·.box))
    add := idxOr 2 (roleOf s.hostRoleTable (·.add))
    sub := idxOr 3 (roleOf s.hostRoleTable (·.sub))
    mul := idxOr 4 (roleOf s.hostRoleTable (·.mul))
    neg := absent 5
    cmp := idxOr 6 (roleOf s.hostRoleTable (·.cmp))
    eq := idxOr 7 (roleOf s.hostRoleTable (·.eq))
    structOf := fun tid => idxOr 8 ((recordOf tt tid).map (·.struct))
    recFields := fun tid => (recordOf tt tid).map (·.fields)
    sigs := fun f => (planOf fns f).map (·.sig)
    sumCtors := fun tid => (sumOf tt tid).map (fun d => d.ctors.map (·.2))
    ctorStruct := fun tid c => idxOr 9 ((sumOf tt tid).bind (fun d => (d.ctors[c]?).map (·.1)))
    sumRoot := fun tid => idxOr 10 ((sumOf tt tid).map (·.root))
    optStruct := lookupTy 11 tt.options
    resStruct := fun t e => idxOr 12 ((tt.results.find? fun x => decide (x.1 = t ∧ x.2.1 = e)).map
      (·.2.2))
    mag := idxOr 13 tt.mag
    str := idxOr 14 tt.str
    strSeg := fun b => idxOr 15 ((tt.strSegs.find? fun x => decide (x.1 = b)).map (·.2))
    strVec := idxOr 16 tt.strVec
    concat := idxOr 17 (stringRole s.stringHostRoles .concat)
    streq := idxOr 18 (stringRole s.stringHostRoles .eq)
    toIndex := idxOr 19 (roleOf s.hostRoleTable (·.toIndex))
    vecStruct := lookupTy 20 tt.vecs
    listStruct := lookupTy 21 tt.lists
    opaqueStruct := lookupNat 22 tt.opaques }

/-! ## The opening rec group, raw and decoded

The type section's first rectype must be an explicit rec group (`0x4e`). Its
subtypes are cut at the lengths `CertDecode.readTypeEntry` consumes, the same
strict decoder the rest of the wall reads the type section with; each entry is
returned as its exact bytes and its decoded form. Entry `k` is the subtype at
type index `k`. -/

def typeSectionStart (n len : Nat) : Option (Nat × Nat) :=
  match CertDecode.modulePayload 1 n len with
  | none => none
  | some (tN, tLen) =>
      match CertDecode.readU tN tLen with
      | none => none
      | some (_, n1, len1) => some (n1, len1)

def readEntriesRaw : Nat → Nat → Nat → Option (List (List Nat × CertDecode.TypeEntry))
  | 0, _, _ => some []
  | k + 1, n, len =>
      match CertDecode.readTypeEntry n len with
      | none => none
      | some (e, n1, len1) =>
          match readEntriesRaw k n1 len1 with
          | none => none
          | some rest => some ((CertDecode.takeBytes (len - len1) n, e) :: rest)

def firstRecGroup (n len : Nat) : Option (List (List Nat × CertDecode.TypeEntry)) :=
  match typeSectionStart n len with
  | none => none
  | some (n1, len1) =>
      if len1 == 0 then none
      else if (n1 &&& 0xff) == 0x4e then
        match CertDecode.readU (n1 >>> 8) (len1 - 1) with
        | none => none
        | some (count, n2, len2) => readEntriesRaw count n2 len2
      else none

/-! ## Expected storage of a source type -/

/-- The decoded value type of a source type's representation (the typed twin
    of `GrammarLower.valTy`): `i32` for Bool, `f64` for Float, `eqref` for the
    subject scratch, and a nullable concrete reference for everything else. An
    index outside the u32 space (an undeclared one) has none. -/
def valTyD (M : MCtx) (t : Ty) : Option CertDecode.ValType :=
  let ref (i : Nat) : Option CertDecode.ValType :=
    if i < 4294967296 then some (.ref 0x63 (Int.ofNat i)) else none
  match t with
  | .int => ref M.carrier
  | .bool => some (.numeric 0x7f)
  | .float => some (.numeric 0x7c)
  | .eqref => some (.abstract 0x6d)
  | .record tid => ref (M.structOf tid)
  | .sum tid => ref (M.sumRoot tid)
  | .option t => ref (M.optStruct t)
  | .result t e => ref (M.resStruct t e)
  | .string => ref M.str
  | .vec t => ref (M.vecStruct t)
  | .list t => ref (M.listStruct t)
  | .opaque tid => ref (M.opaqueStruct tid)

def storagesOf (M : MCtx) (ts : List Ty) : Option (List CertDecode.StorageType) :=
  ts.mapM fun t => (valTyD M t).map .val

def structStorages : CertDecode.TypeEntry → Option (List CertDecode.StorageType)
  | ⟨_, .structType fs⟩ => some (fs.map (·.storage))
  | _ => none

/-- The entry at `idx` of the group is a struct whose fields store exactly
    the representations of `ts` (mutability aside: the grammar never writes a
    field). -/
def structIs (M : MCtx) (grp : List (List Nat × CertDecode.TypeEntry)) (idx : Nat)
    (ts : List Ty) : Bool :=
  match grp[idx]?, storagesOf M ts with
  | some (_, e), some ss => structStorages e == some ss
  | _, _ => false

def arrayIs (grp : List (List Nat × CertDecode.TypeEntry)) (idx : Nat)
    (st : CertDecode.StorageType) : Bool :=
  match grp[idx]? with
  | some (_, ⟨_, .arrayType f⟩) => f.storage == st
  | _ => false

def refTo (i : Nat) : CertDecode.StorageType := .val (.ref 0x63 (Int.ofNat i))

/-! ## The pins -/

/-- The carrier declaration is the byte-derived carrier state, and a present
    carrier's limb field names the declared magnitude array, itself an `i64`
    array of the opening group. -/
def carrierConfirmed (n len : Nat) (grp : List (List Nat × CertDecode.TypeEntry))
    (tt : TypeTable) : Bool :=
  match CertDecode.carrierState n len, tt.carrier, tt.mag with
  | some (some c), some c', some m =>
      c == c' && decide (c < 4294967296) && decide (m < 4294967296) &&
        arrayIs grp m (.val (.numeric 0x7e)) &&
        (match grp[c]? with
         | some (_, ⟨_, .structType fs⟩) => (fs[1]?).map (·.storage) == some (refTo m)
         | _ => false)
  | some none, none, none => true
  | _, _, _ => false

def recordConfirmed (M : MCtx) (grp : List (List Nat × CertDecode.TypeEntry))
    (r : RecordDecl) : Bool :=
  match r.fields with
  | [f] => valTyD M f == some (.ref 0x63 (Int.ofNat r.struct)) && decide (r.struct < 4294967296)
  | _ => decide (2 ≤ r.fields.length) && structIs M grp r.struct r.fields

def rootIs (grp : List (List Nat × CertDecode.TypeEntry)) (idx : Nat) : Bool :=
  match grp[idx]? with
  | some (_, e) => e == ⟨.sub [], .structType []⟩
  | none => false

def ctorIs (M : MCtx) (grp : List (List Nat × CertDecode.TypeEntry)) (root : Nat)
    (c : Nat × List Ty) : Bool :=
  structIs M grp c.1 c.2 &&
    (match grp[c.1]? with
     | some (_, e) => e.form == .subFinal [root]
     | none => false)

def sumConfirmed (M : MCtx) (grp : List (List Nat × CertDecode.TypeEntry)) (d : SumDecl) :
    Bool :=
  rootIs grp d.root && d.ctors.all (ctorIs M grp d.root) && sumOk M d.tid &&
    S3Pin M d.tid d.ctors.length (grp.map (·.1))

/-- Every struct index a declaration owns (a newtype owns none). -/
def ownedStructs (tt : TypeTable) : List Nat :=
  (tt.records.filter (fun r => decide (2 ≤ r.fields.length))).map (·.struct) ++
  (tt.sums.map fun d => d.root :: d.ctors.map (·.1)).flatten ++
  tt.options.map (·.2) ++ tt.results.map (·.2.2) ++ tt.lists.map (·.2) ++
  tt.vecs.map (·.2) ++ tt.carrier.toList ++ tt.mag.toList ++ tt.str.toList ++
  tt.strVec.toList

def natNodup : List Nat → Bool
  | [] => true
  | x :: xs => !xs.contains x && natNodup xs

/-- Every declared type id appears once, so the lookups `mctxOf` makes are the
    declarations the pins confirm. -/
def keysUnique (tt : TypeTable) : Bool :=
  natNodup (tt.records.map (·.tid)) && natNodup (tt.sums.map (·.tid))

/-- The whole type table against the module bytes (S-2, S-3). -/
def typeTableConfirmed (n len : Nat) (s : Subject) (tt : TypeTable) (fns : List FnEntry) :
    Bool :=
  let M := mctxOf s tt fns
  (CertDecode.decodeTypes n len).isSome &&
  match firstRecGroup n len with
  | none => false
  | some grp =>
      keysUnique tt &&
      carrierConfirmed n len grp tt &&
      natNodup (ownedStructs tt) &&
      tt.records.all (recordConfirmed M grp) &&
      tt.sums.all (sumConfirmed M grp) &&
      tt.options.all (fun o => structIs M grp o.2 [.bool, o.1]) &&
      tt.results.all (fun r => structIs M grp r.2.2 [.bool, r.1, r.2.1]) &&
      tt.lists.all (fun l => structIs M grp l.2 [l.1, .list l.1]) &&
      tt.vecs.all (fun v => match storagesOf M [v.1] with
        | some [st] => arrayIs grp v.2 st
        | _ => false) &&
      tt.opaques.all (fun o => decide (o.2 < grp.length)) &&
      (match tt.str with
       | some i => arrayIs grp i (.packed 0x78)
       | none => true) &&
      (match tt.strVec, tt.str with
       | some v, some i => arrayIs grp v (refTo i)
       | none, _ => true
       | some _, none => false)

/-- S-11 over the module's data section: every declared literal-to-segment
    entry names a passive segment holding exactly its bytes, and every string
    literal of every plan names such a segment. -/
def dataConfirmed (n len : Nat) (s : Subject) (tt : TypeTable) (fns : List FnEntry) : Bool :=
  match CertDecode.decodeData n len with
  | some segs =>
      tt.strSegs.all (fun x => segs[x.2]? == some x.1) &&
      fns.all fun e => DataPin (mctxOf s tt fns) segs e.plan
  | none => false

end AverCert.TypeTable
