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
        if self.tag() != other.tag() {
            return false;
        }
        match self.tag() {
            TAG_INT => self.as_int(arena) == other.as_int(arena),
            TAG_IMMEDIATE => false,
            TAG_WRAPPER => unreachable!("wrapper comparison handled above"),
            TAG_STRING => {
                arena.get_string(self.arena_index()) == arena.get_string(other.arena_index())
            }
            TAG_LIST => {
                let a_idx = self.arena_index();
                let b_idx = other.arena_index();
                arena.list_len(a_idx) == arena.list_len(b_idx)
                    && (0..arena.list_len(a_idx)).all(|i| {
                        arena
                            .list_get(a_idx, i)
                            .zip(arena.list_get(b_idx, i))
                            .is_some_and(|(x, y)| x.eq_in(y, arena))
                    })
            }
            TAG_TUPLE => {
                let a = arena.get_tuple(self.arena_index());
                let b = arena.get_tuple(other.arena_index());
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.eq_in(*y, arena))
            }
            TAG_MAP => {
                let a = arena.get_map(self.arena_index());
                let b = arena.get_map(other.arena_index());
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
            TAG_VARIANT => {
                let (ta, va, fa) = arena.get_variant(self.arena_index());
                let (tb, vb, fb) = arena.get_variant(other.arena_index());
                ta == tb
                    && va == vb
                    && fa.len() == fb.len()
                    && fa.iter().zip(fb).all(|(a, b)| a.eq_in(*b, arena))
            }
            TAG_FN => self.arena_index() == other.arena_index(),
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
            (TAG_WRAPPER as u8).hash(state);
            kind.hash(state);
            inner.hash_in(state, arena);
            return;
        }
        let tag = self.tag();
        (tag as u8).hash(state);
        match tag {
            TAG_INT => self.as_int(arena).hash(state),
            TAG_IMMEDIATE => self.payload().hash(state),
            TAG_WRAPPER => unreachable!("wrapper hashing handled above"),
            TAG_STRING => arena.get_string(self.arena_index()).hash(state),
            TAG_LIST => {
                let list_idx = self.arena_index();
                arena.list_len(list_idx).hash(state);
                for item in arena.list_to_vec(list_idx) {
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
            TAG_RECORD => {
                let (tid, fields) = arena.get_record(self.arena_index());
                tid.hash(state);
                for f in fields {
                    f.hash_in(state, arena);
                }
            }
            TAG_VARIANT => {
                let (tid, vid, fields) = arena.get_variant(self.arena_index());
                tid.hash(state);
                vid.hash(state);
                for f in fields {
                    f.hash_in(state, arena);
                }
            }
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
        match self.tag() {
            TAG_INT => self.as_int(arena).to_string(),
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => "false".into(),
                IMM_TRUE => "true".into(),
                IMM_UNIT => "Unit".into(),
                IMM_NONE => "Option.None".into(),
                _ => "??".into(),
            },
            TAG_WRAPPER => unreachable!("wrapper repr handled above"),
            TAG_STRING => arena.get_string(self.arena_index()).to_string(),
            TAG_LIST => {
                let parts: Vec<_> = arena
                    .list_to_vec(self.arena_index())
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
            TAG_MAP => {
                let map = arena.get_map(self.arena_index());
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
            TAG_VARIANT => {
                let (tid, vid, fields) = arena.get_variant(self.arena_index());
                let vname = arena.get_variant_name(tid, vid);
                if fields.is_empty() {
                    vname.to_string()
                } else {
                    let parts: Vec<_> = fields.iter().map(|v| v.repr_inner(arena)).collect();
                    format!("{}({})", vname, parts.join(", "))
                }
            }
            TAG_FN => format!("<fn {}>", arena.get_fn(self.arena_index()).name),
            TAG_BUILTIN => format!("<builtin {}>", arena.get_builtin(self.arena_index())),
            TAG_NAMESPACE => {
                let (name, _) = arena.get_namespace(self.arena_index());
                format!("<type {}>", name)
            }
            _ => "??".into(),
        }
    }

    fn repr_inner(self, arena: &Arena) -> String {
        if self.is_string() {
            return format!("\"{}\"", arena.get_string(self.arena_index()));
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
