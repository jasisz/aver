use super::*;
use alloc::collections::BTreeSet;
use alloc::string::ToString;

/// How many pairs of arena values one `==` compares before it starts again
/// remembering what it found equal. A comparison within it pays a counter;
/// one past it repeats at most this much work.
const EQ_BUDGET: u32 = 1024;

/// How a comparison goes on: within its budget, or with memory.
///
/// One value can sit in many fields of an arena (`Pair(t, t)` stores `t`
/// twice), so two equal values built apart can share their subterms
/// internally while sharing nothing with each other. A walk meets such a
/// subterm once for every path to it: the work is the size of the unfolded
/// tree, exponential in the depth of the sharing. The comparison with memory
/// looks a pair up before it compares it, and remembers it once it is found
/// equal. The arena is not changed while `==` runs, so the bits of a value
/// name it for the whole comparison, and the answer for a pair of bits is
/// always the same.
struct EqWalk<'a> {
    left: u32,
    equal: Option<&'a mut BTreeSet<(u64, u64)>>,
}

/// What [`NanValue::eq_open`] makes of a pair.
enum EqOpen<'a> {
    /// The answer, without looking further.
    Same(bool),
    /// The parts that decide it, and with memory the pair (as bits) to
    /// remember once they are all found equal.
    Parts(EqWork<'a>, Option<(u64, u64)>),
}

/// Pairs of values a comparison still has to find equal, left to right.
enum EqWork<'a> {
    /// Two values.
    Pair(NanValue, NanValue),
    /// Fields, items or elements, paired by position.
    Items(&'a [NanValue], &'a [NanValue]),
    /// The elements of two lists of length `len`, from the one at `at`.
    List(NanValue, NanValue, usize, usize),
    /// The values of two maps paired by key, the next one last.
    Values(Vec<(NanValue, NanValue)>),
    /// A pair whose parts have all been found equal by the time this is
    /// reached.
    Remember((u64, u64)),
}

impl EqWork<'_> {
    /// The next pair to compare: `None` when there is none left, `Some(None)`
    /// when two lists of one length do not have an element at one position.
    fn next_pair<T: ArenaTypes>(
        &mut self,
        arena: &Arena<T>,
    ) -> Option<Option<(NanValue, NanValue)>> {
        match self {
            EqWork::Pair(a, b) => {
                let pair = (*a, *b);
                *self = EqWork::Items(&[], &[]);
                Some(Some(pair))
            }
            EqWork::Items(a, b) => {
                let ((x, rest_a), (y, rest_b)) = (a.split_first()?, b.split_first()?);
                let pair = (*x, *y);
                (*a, *b) = (rest_a, rest_b);
                Some(Some(pair))
            }
            EqWork::List(a, b, at, len) => {
                if *at >= *len {
                    return None;
                }
                let position = *at;
                *at += 1;
                Some(
                    arena
                        .list_get_value(*a, position)
                        .zip(arena.list_get_value(*b, position)),
                )
            }
            EqWork::Values(values) => values.pop().map(Some),
            EqWork::Remember(_) => None,
        }
    }

    fn is_done(&self) -> bool {
        match self {
            EqWork::Pair(..) => false,
            EqWork::Items(a, b) => a.is_empty() || b.is_empty(),
            EqWork::List(_, _, at, len) => at >= len,
            EqWork::Values(values) => values.is_empty(),
            EqWork::Remember(_) => true,
        }
    }
}

impl NanValue {
    #[inline]
    pub fn eq_in<T: ArenaTypes>(self, other: Self, arena: &Arena<T>) -> bool {
        self.eq_top::<T, false>(other, arena)
    }

    /// A top-level `==`: within [`EQ_BUDGET`] as it always was, from the
    /// start with memory if that runs out. Kept out of line so the VM's
    /// dispatch loop holds one call for `==`, not the comparison.
    #[inline(never)]
    fn eq_top<T: ArenaTypes, const KEY: bool>(self, other: Self, arena: &Arena<T>) -> bool {
        let mut walk = EqWalk {
            left: EQ_BUDGET,
            equal: None,
        };
        let same = self.eq_as::<T, KEY>(other, arena, &mut walk);
        if walk.left > 0 {
            return same;
        }
        self.eq_remembering::<T, KEY>(other, arena)
    }

    #[cold]
    #[inline(never)]
    fn eq_remembering<T: ArenaTypes, const KEY: bool>(self, other: Self, arena: &Arena<T>) -> bool {
        let mut equal = BTreeSet::new();
        let mut walk = EqWalk {
            left: 0,
            equal: Some(&mut equal),
        };
        self.eq_as::<T, KEY>(other, arena, &mut walk)
    }

    /// Whether `self` and `other` are the same map key.
    ///
    /// This is `eq_in` everywhere except on a float, where it is the relation
    /// [`NanValue::hash_in`] hashes by: both zeroes are one key and a NaN is
    /// the key it was stored under. `==` cannot serve there, because IEEE 754
    /// makes a NaN unequal to itself and a map would never find it again. The
    /// checker refuses a float in key position, so this only decides values
    /// the runtime is handed from outside the language.
    #[inline]
    pub fn key_eq_in<T: ArenaTypes>(self, other: Self, arena: &Arena<T>) -> bool {
        self.eq_top::<T, true>(other, arena)
    }

    /// Compare on a work list rather than the Rust call stack, so a value of
    /// a recursive sum a million levels deep compares like a shallow one.
    ///
    /// The walk is the depth-first, left-to-right one a recursive comparison
    /// makes, and it stops at the first difference. A pair is opened by
    /// [`NanValue::eq_open`]; what it opens to is compared next, and the parts
    /// it has left are kept on `stack` until that is done. With memory, an
    /// [`EqWork::Remember`] goes under the parts of a pair, so the pair is
    /// remembered when all of them have been found equal and not before, as it
    /// would be on the way back up a recursion.
    #[inline(always)]
    fn eq_as<T: ArenaTypes, const KEY: bool>(
        self,
        other: Self,
        arena: &Arena<T>,
        walk: &mut EqWalk<'_>,
    ) -> bool {
        match self.eq_open::<T, KEY>(other, arena, walk) {
            EqOpen::Same(same) => same,
            EqOpen::Parts(parts, remember) => Self::eq_walk::<T, KEY>(parts, remember, arena, walk),
        }
    }

    /// [`NanValue::eq_as`] of a pair that has opened to `parts`.
    fn eq_walk<T: ArenaTypes, const KEY: bool>(
        parts: EqWork<'_>,
        remember: Option<(u64, u64)>,
        arena: &Arena<T>,
        walk: &mut EqWalk<'_>,
    ) -> bool {
        let mut stack = Vec::new();
        if let Some(pair) = remember {
            stack.push(EqWork::Remember(pair));
        }
        let mut next = Some(parts);
        loop {
            let mut work = match next.take().or_else(|| stack.pop()) {
                Some(EqWork::Remember(pair)) => {
                    if let Some(seen) = walk.equal.as_deref_mut() {
                        seen.insert(pair);
                    }
                    continue;
                }
                Some(work) => work,
                None => return true,
            };
            while let Some(pair) = work.next_pair(arena) {
                let Some((a, b)) = pair else {
                    return false;
                };
                match a.eq_open::<T, KEY>(b, arena, walk) {
                    EqOpen::Same(true) => {}
                    EqOpen::Same(false) => return false,
                    EqOpen::Parts(parts, remember) => {
                        if !work.is_done() {
                            stack.push(work);
                        }
                        if let Some(pair) = remember {
                            stack.push(EqWork::Remember(pair));
                        }
                        next = Some(parts);
                        break;
                    }
                }
            }
        }
    }

    /// One step of [`NanValue::eq_as`]: the answer for a pair, or the parts
    /// that decide it, and with memory the pair to remember once they have.
    #[inline(always)]
    fn eq_open<'a, T: ArenaTypes, const KEY: bool>(
        self,
        other: Self,
        arena: &'a Arena<T>,
        walk: &mut EqWalk<'_>,
    ) -> EqOpen<'a> {
        // Float comparison goes first and never short-circuits on
        // bit-pattern equality. IEEE 754 says `NaN == NaN` is false
        // and `+0.0 == -0.0` is true; both rules disagree with raw
        // bit comparison, so we always defer to `f64::PartialEq` for
        // floats. This keeps the VM's `==` aligned with what Rust
        // codegen and WASM emit (both use IEEE 754 directly).
        if self.is_float() || other.is_float() {
            if !(self.is_float() && other.is_float()) {
                return EqOpen::Same(false);
            }
            let (a, b) = (self.as_float(), other.as_float());
            return EqOpen::Same(a == b || (KEY && a.to_bits() == b.to_bits()));
        }
        if self.0 == other.0 {
            return EqOpen::Same(true);
        }
        let mut remember = None;
        if self.may_hold_heap_index() && other.may_hold_heap_index() {
            if let Some(seen) = walk.equal.as_deref() {
                // With memory: a pair is looked up, and remembered once found
                // equal.
                let pair = (self.0, other.0);
                if seen.contains(&pair) {
                    return EqOpen::Same(true);
                }
                remember = Some(pair);
            } else if walk.left == 0 {
                // Out of budget: the answer is thrown away and found again.
                return EqOpen::Same(false);
            } else {
                walk.left -= 1;
            }
        }
        match self.eq_parts::<T, KEY>(other, arena) {
            EqOpen::Same(same) => {
                if same && let (Some(pair), Some(seen)) = (remember, walk.equal.as_deref_mut()) {
                    seen.insert(pair);
                }
                EqOpen::Same(same)
            }
            EqOpen::Parts(parts, _) => EqOpen::Parts(parts, remember),
        }
    }

    /// [`NanValue::eq_open`] of two values that are not one: the answer, if
    /// it does not depend on their parts, or else the parts.
    fn eq_parts<'a, T: ArenaTypes, const KEY: bool>(
        self,
        other: Self,
        arena: &'a Arena<T>,
    ) -> EqOpen<'a> {
        let same = EqOpen::Same;
        let parts = |parts: EqWork<'a>| EqOpen::Parts(parts, None);
        match (self.wrapper_parts(arena), other.wrapper_parts(arena)) {
            (Some((self_kind, self_inner)), Some((other_kind, other_inner))) => {
                return match self_kind == other_kind {
                    true => parts(EqWork::Pair(self_inner, other_inner)),
                    false => same(false),
                };
            }
            (Some(_), None) | (None, Some(_)) => return same(false),
            (None, None) => {}
        }
        // Inline variant fast path (no arena indirection)
        if self.tag() == TAG_INLINE_VARIANT || other.tag() == TAG_INLINE_VARIANT {
            let s_ctor = self.variant_ctor_id(arena);
            let o_ctor = other.variant_ctor_id(arena);
            return match (s_ctor, o_ctor) {
                (Some(sc), Some(oc)) if sc == oc => {
                    // Same constructor — compare field(s)
                    if self.tag() == TAG_INLINE_VARIANT && other.tag() == TAG_INLINE_VARIANT {
                        parts(EqWork::Pair(
                            self.inline_variant_inner(),
                            other.inline_variant_inner(),
                        ))
                    } else {
                        // Cross-representation: extract the single field from each side
                        parts(EqWork::Pair(
                            self.variant_single_field(arena),
                            other.variant_single_field(arena),
                        ))
                    }
                }
                _ => same(false),
            };
        }
        match (self.variant_parts(arena), other.variant_parts(arena)) {
            (Some((st, sv, sf)), Some((ot, ov, of))) => {
                return match st == ot && sv == ov && sf.len() == of.len() {
                    true => parts(EqWork::Items(sf, of)),
                    false => same(false),
                };
            }
            (Some(_), None) | (None, Some(_)) => return same(false),
            (None, None) => {}
        }
        if self.is_string() || other.is_string() {
            return same(
                self.is_string()
                    && other.is_string()
                    && arena.get_string_value(self) == arena.get_string_value(other),
            );
        }
        if self.tag() != other.tag() {
            return same(false);
        }
        let items = |a: &'a [NanValue], b: &'a [NanValue]| match a.len() == b.len() {
            true => parts(EqWork::Items(a, b)),
            false => same(false),
        };
        match self.tag() {
            TAG_INT => same(int_ref_eq(self.int_ref(arena), other.int_ref(arena))),
            TAG_IMMEDIATE | TAG_NONE => same(false),
            TAG_STRING => same(arena.get_string_value(self) == arena.get_string_value(other)),
            TAG_LIST => {
                let len = arena.list_len_value(self);
                match len == arena.list_len_value(other) {
                    true => parts(EqWork::List(self, other, 0, len)),
                    false => same(false),
                }
            }
            TAG_TUPLE => items(
                arena.get_tuple(self.arena_index()),
                arena.get_tuple(other.arena_index()),
            ),
            TAG_VECTOR => items(arena.vector_ref_value(self), arena.vector_ref_value(other)),
            TAG_MAP => {
                let a = arena.map_ref_value(self);
                let b = arena.map_ref_value(other);
                if a.len() != b.len() {
                    return same(false);
                }
                // A slot names a key only up to its hash, so every pairing is
                // confirmed on the key itself. The slot `a` keeps an entry in
                // is where `b` keeps it too unless one of them holds a second
                // key with the same hash; only then is the key looked up.
                let mut values = Vec::with_capacity(a.len());
                for (slot, (k, v1)) in a.iter() {
                    let found = match b.get(slot) {
                        Some((k2, v2)) if k.key_eq_in(*k2, arena) => Some(*v2),
                        _ => map_get_value::<T>(b, *k, arena),
                    };
                    match found {
                        Some(v2) => values.push((*v1, v2)),
                        None => return same(false),
                    }
                }
                values.reverse();
                parts(EqWork::Values(values))
            }
            TAG_RECORD => {
                let (ta, fa) = arena.get_record(self.arena_index());
                let (tb, fb) = arena.get_record(other.arena_index());
                match ta == tb {
                    true => items(fa, fb),
                    false => same(false),
                }
            }
            TAG_VARIANT | TAG_INLINE_VARIANT => {
                unreachable!("variant comparison handled above")
            }
            TAG_SYMBOL => same(self.bits() == other.bits()),
            _ => same(false),
        }
    }
    pub fn hash_in<T: ArenaTypes, H: core::hash::Hasher>(self, state: &mut H, arena: &Arena<T>) {
        use core::hash::Hash;
        if self.is_float() {
            1u8.hash(state);
            let f = self.as_float();
            let bits = if f == 0.0 {
                0.0f64.to_bits()
            } else {
                f.to_bits()
            };
            bits.hash(state);
            return;
        }
        if let Some((kind, inner)) = self.wrapper_parts(arena) {
            match kind {
                WRAP_SOME => (TAG_SOME as u8).hash(state),
                WRAP_OK => (TAG_OK as u8).hash(state),
                WRAP_ERR => (TAG_ERR as u8).hash(state),
                _ => (0xFFu8).hash(state),
            }
            inner.hash_in(state, arena);
            return;
        }
        if let Some((tid, vid, inner)) = self.inline_variant_info(arena) {
            (TAG_VARIANT as u8).hash(state);
            tid.hash(state);
            vid.hash(state);
            inner.hash_in(state, arena);
            return;
        }
        if let Some((tid, vid, fields)) = self.variant_parts(arena) {
            (TAG_VARIANT as u8).hash(state);
            tid.hash(state);
            vid.hash(state);
            for field in fields {
                field.hash_in(state, arena);
            }
            return;
        }
        if self.is_string() {
            (TAG_STRING as u8).hash(state);
            arena.get_string_value(self).hash(state);
            return;
        }
        let tag = self.tag();
        (tag as u8).hash(state);
        match tag {
            TAG_INT => int_ref_hash(self.int_ref(arena), state),
            TAG_IMMEDIATE => self.payload().hash(state),
            TAG_NONE => 0u8.hash(state),
            TAG_STRING => arena.get_string_value(self).hash(state),
            TAG_LIST => {
                arena.list_len_value(self).hash(state);
                for item in arena.list_to_vec_value(self) {
                    item.hash_in(state, arena);
                }
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                items.len().hash(state);
                for item in items {
                    item.hash_in(state, arena);
                }
            }
            TAG_VECTOR => {
                let items = arena.vector_ref_value(self);
                items.len().hash(state);
                for item in items {
                    item.hash_in(state, arena);
                }
            }
            TAG_RECORD => {
                let (tid, fields) = arena.get_record(self.arena_index());
                tid.hash(state);
                for f in fields {
                    f.hash_in(state, arena);
                }
            }
            TAG_VARIANT | TAG_INLINE_VARIANT => {
                unreachable!("variant hashing handled above")
            }
            TAG_SYMBOL => self.bits().hash(state),
            _ => self.0.hash(state),
        }
    }

    pub fn repr<T: ArenaTypes>(self, arena: &Arena<T>) -> String {
        if self.is_float() {
            return format!("{}", self.as_float());
        }
        if let Some((kind, inner)) = self.wrapper_parts(arena) {
            let ir = inner.repr_inner(arena);
            return match kind {
                WRAP_SOME => format!("Option.Some({})", ir),
                WRAP_OK => format!("Result.Ok({})", ir),
                WRAP_ERR => format!("Result.Err({})", ir),
                _ => String::from("??"),
            };
        }
        if let Some((tid, vid, inner)) = self.inline_variant_info(arena) {
            let vname = arena.get_variant_name(tid, vid);
            return format!("{}({})", vname, inner.repr_inner(arena));
        }
        if let Some((tid, vid, fields)) = self.variant_parts(arena) {
            let vname = arena.get_variant_name(tid, vid);
            return if fields.is_empty() {
                String::from(vname)
            } else {
                let parts: Vec<_> = fields.iter().map(|v| v.repr_inner(arena)).collect();
                format!("{}({})", vname, parts.join(", "))
            };
        }
        if self.is_string() {
            return arena.get_string_value(self).to_string();
        }
        match self.tag() {
            TAG_INT => match self.int_ref(arena) {
                ArenaIntRef::Small(n) => format!("{}", n),
                ArenaIntRef::Big(b) => format!("{}", b),
            },
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => String::from("false"),
                IMM_TRUE => String::from("true"),
                IMM_UNIT => String::from("Unit"),
                _ => String::from("??"),
            },
            TAG_NONE => String::from("Option.None"),
            TAG_STRING => arena.get_string_value(self).to_string(),
            TAG_LIST => {
                let parts: Vec<_> = arena
                    .list_to_vec_value(self)
                    .into_iter()
                    .map(|v| v.repr_inner(arena))
                    .collect();
                format!("[{}]", parts.join(", "))
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                let parts: Vec<_> = items.iter().map(|v| v.repr_inner(arena)).collect();
                format!("({})", parts.join(", "))
            }
            TAG_VECTOR => {
                let items = arena.vector_ref_value(self);
                let parts: Vec<_> = items.iter().map(|v| v.repr_inner(arena)).collect();
                format!("Vector[{}]", parts.join(", "))
            }
            TAG_MAP => {
                let map = arena.map_ref_value(self);
                let mut pairs: Vec<_> = map
                    .values()
                    .map(|(k, v)| (k.repr_inner(arena), v.repr_inner(arena)))
                    .collect();
                pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
                let parts: Vec<_> = pairs
                    .into_iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                format!("{{{}}}", parts.join(", "))
            }
            TAG_RECORD => {
                let (tid, fields) = arena.get_record(self.arena_index());
                let name = arena.get_type_name(tid);
                let fnames = arena.get_field_names(tid);
                let parts: Vec<_> = fnames
                    .iter()
                    .zip(fields)
                    .map(|(n, v)| format!("{}: {}", n, v.repr_inner(arena)))
                    .collect();
                format!("{}({})", name, parts.join(", "))
            }
            TAG_VARIANT | TAG_INLINE_VARIANT => unreachable!("variant repr handled above"),
            TAG_SYMBOL => match self.symbol_kind() {
                SYMBOL_FN => format!("<fn {}>", arena.get_fn(self.symbol_index()).name()),
                SYMBOL_BUILTIN => format!("<builtin {}>", arena.get_builtin(self.symbol_index())),
                SYMBOL_NAMESPACE => {
                    let (name, _) = arena.get_namespace(self.symbol_index());
                    format!("<type {}>", name)
                }
                SYMBOL_NULLARY_VARIANT => unreachable!("variant repr handled above"),
                _ => String::from("??"),
            },
            _ => String::from("??"),
        }
    }

    fn repr_inner<T: ArenaTypes>(self, arena: &Arena<T>) -> String {
        if self.is_string() {
            return format!("\"{}\"", arena.get_string_value(self));
        }
        self.repr(arena)
    }

    pub fn display<T: ArenaTypes>(self, arena: &Arena<T>) -> Option<String> {
        if self.is_unit() {
            None
        } else {
            Some(self.repr(arena))
        }
    }
}

/// Equality over the three integer representations. Canonical form guarantees
/// a `Small` and a `Big` are never numerically equal, so cross-variant pairs
/// are always `false`. Two `Big`s built differently but equal in value compare
/// equal here (the structural-key invariant Map/Set keying relies on).
fn int_ref_eq(a: ArenaIntRef<'_>, b: ArenaIntRef<'_>) -> bool {
    match (a, b) {
        (ArenaIntRef::Small(x), ArenaIntRef::Small(y)) => x == y,
        (ArenaIntRef::Big(x), ArenaIntRef::Big(y)) => x == y,
        _ => false,
    }
}

/// Hash over the integer representations, consistent with `int_ref_eq`: a
/// `Small` and a value-equal `Big` cannot coexist (canonical form), and two
/// equal `Big`s hash identically regardless of arena placement.
fn int_ref_hash<H: core::hash::Hasher>(v: ArenaIntRef<'_>, state: &mut H) {
    use core::hash::Hash;
    match v {
        ArenaIntRef::Small(n) => n.hash(state),
        ArenaIntRef::Big(b) => b.hash(state),
    }
}
