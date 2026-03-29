use super::*;

impl NanValue {
    pub fn eq_in(self, other: Self, arena: &Arena) -> bool {
        if self.0 == other.0 {
            return true;
        }
        if self.is_float() != other.is_float() {
            return false;
        }
        if self.is_float() {
            return self.as_float() == other.as_float();
        }
        match (self.wrapper_parts(arena), other.wrapper_parts(arena)) {
            (Some((self_kind, self_inner)), Some((other_kind, other_inner))) => {
                return self_kind == other_kind && self_inner.eq_in(other_inner, arena);
            }
            (Some(_), None) | (None, Some(_)) => return false,
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
                        self.inline_variant_inner()
                            .eq_in(other.inline_variant_inner(), arena)
                    } else {
                        // Cross-representation: extract the single field from each side
                        let sf = self.variant_single_field(arena);
                        let of = other.variant_single_field(arena);
                        sf.eq_in(of, arena)
                    }
                }
                (Some(_), Some(_)) => false,
                _ => false,
            };
        }
        match (self.variant_parts(arena), other.variant_parts(arena)) {
            (Some((st, sv, sf)), Some((ot, ov, of))) => {
                return st == ot
                    && sv == ov
                    && sf.len() == of.len()
                    && sf.iter().zip(of).all(|(a, b)| a.eq_in(*b, arena));
            }
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }
        if self.is_string() || other.is_string() {
            return self.is_string()
                && other.is_string()
                && arena.get_string_value(self) == arena.get_string_value(other);
        }
        if self.tag() != other.tag() {
            return false;
        }
        match self.tag() {
            TAG_INT => self.as_int(arena) == other.as_int(arena),
            TAG_IMMEDIATE | TAG_NONE => false,
            TAG_STRING => arena.get_string_value(self) == arena.get_string_value(other),
            TAG_LIST => {
                let a_len = arena.list_len_value(self);
                let b_len = arena.list_len_value(other);
                a_len == b_len
                    && (0..a_len).all(|i| {
                        arena
                            .list_get_value(self, i)
                            .zip(arena.list_get_value(other, i))
                            .is_some_and(|(x, y)| x.eq_in(y, arena))
                    })
            }
            TAG_TUPLE => {
                let a = arena.get_tuple(self.arena_index());
                let b = arena.get_tuple(other.arena_index());
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.eq_in(*y, arena))
            }
            TAG_VECTOR => {
                let a = arena.vector_ref_value(self);
                let b = arena.vector_ref_value(other);
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.eq_in(*y, arena))
            }
            TAG_MAP => {
                let a = arena.map_ref_value(self);
                let b = arena.map_ref_value(other);
                a.len() == b.len()
                    && a.iter()
                        .all(|(k, (_, v1))| b.get(k).is_some_and(|(_, v2)| v1.eq_in(*v2, arena)))
            }
            TAG_RECORD => {
                let (ta, fa) = arena.get_record(self.arena_index());
                let (tb, fb) = arena.get_record(other.arena_index());
                ta == tb
                    && fa.len() == fb.len()
                    && fa.iter().zip(fb).all(|(a, b)| a.eq_in(*b, arena))
            }
            TAG_VARIANT | TAG_INLINE_VARIANT => {
                unreachable!("variant comparison handled above")
            }
            TAG_SYMBOL => self.bits() == other.bits(),
            _ => false,
        }
    }

    pub fn hash_in<H: std::hash::Hasher>(self, state: &mut H, arena: &Arena) {
        use std::hash::Hash;
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
            TAG_INT => self.as_int(arena).hash(state),
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

    pub fn repr(self, arena: &Arena) -> String {
        if self.is_float() {
            return self.as_float().to_string();
        }
        if let Some((kind, inner)) = self.wrapper_parts(arena) {
            let ir = inner.repr_inner(arena);
            return match kind {
                WRAP_SOME => format!("Option.Some({})", ir),
                WRAP_OK => format!("Result.Ok({})", ir),
                WRAP_ERR => format!("Result.Err({})", ir),
                _ => "??".into(),
            };
        }
        if let Some((tid, vid, inner)) = self.inline_variant_info(arena) {
            let vname = arena.get_variant_name(tid, vid);
            return format!("{}({})", vname, inner.repr_inner(arena));
        }
        if let Some((tid, vid, fields)) = self.variant_parts(arena) {
            let vname = arena.get_variant_name(tid, vid);
            return if fields.is_empty() {
                vname.to_string()
            } else {
                let parts: Vec<_> = fields.iter().map(|v| v.repr_inner(arena)).collect();
                format!("{}({})", vname, parts.join(", "))
            };
        }
        if self.is_string() {
            return arena.get_string_value(self).to_string();
        }
        match self.tag() {
            TAG_INT => self.as_int(arena).to_string(),
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => "false".into(),
                IMM_TRUE => "true".into(),
                IMM_UNIT => "Unit".into(),
                _ => "??".into(),
            },
            TAG_NONE => "Option.None".into(),
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
                SYMBOL_FN => format!("<fn {}>", arena.get_fn(self.symbol_index()).name),
                SYMBOL_BUILTIN => format!("<builtin {}>", arena.get_builtin(self.symbol_index())),
                SYMBOL_NAMESPACE => {
                    let (name, _) = arena.get_namespace(self.symbol_index());
                    format!("<type {}>", name)
                }
                SYMBOL_NULLARY_VARIANT => unreachable!("variant repr handled above"),
                _ => "??".into(),
            },
            _ => "??".into(),
        }
    }

    fn repr_inner(self, arena: &Arena) -> String {
        if self.is_string() {
            return format!("\"{}\"", arena.get_string_value(self));
        }
        self.repr(arena)
    }

    pub fn display(self, arena: &Arena) -> Option<String> {
        if self.is_unit() {
            None
        } else {
            Some(self.repr(arena))
        }
    }
}
