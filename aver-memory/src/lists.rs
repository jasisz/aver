use alloc::vec;
use super::*;

impl<T: ArenaTypes> Arena<T> {
    pub fn push_list_prepend(&mut self, head: NanValue, tail: NanValue) -> u32 {
        debug_assert!(tail.is_list());
        let len = self.list_len_value(tail) + 1;
        self.push(ArenaEntry::List(ArenaList::Prepend { head, tail, len }))
    }

    pub fn push_list_append(&mut self, list: NanValue, value: NanValue) -> u32 {
        debug_assert!(list.is_list());
        if self.list_is_empty_value(list) {
            return self.push_list(vec![value]);
        }

        match self.get_list(list.arena_index()).clone() {
            ArenaList::Flat { items, start } => {
                let slice = &items[start..];
                if slice.len() < LIST_APPEND_CHUNK_LIMIT {
                    let mut out = Vec::with_capacity(slice.len() + 1);
                    out.extend(slice.iter().copied());
                    out.push(value);
                    return self.push_list(out);
                }
            }
            ArenaList::Concat { left, right, len } => {
                if let ArenaList::Flat { items, start } = self.get_list(right.arena_index()).clone()
                {
                    let slice = &items[start..];
                    if slice.len() < LIST_APPEND_CHUNK_LIMIT {
                        let mut out = Vec::with_capacity(slice.len() + 1);
                        out.extend(slice.iter().copied());
                        out.push(value);
                        let right_idx = self.push_list(out);
                        return self.push(ArenaEntry::List(ArenaList::Concat {
                            left,
                            right: NanValue::new_list(right_idx),
                            len: len + 1,
                        }));
                    }
                }
            }
            ArenaList::Prepend { .. } | ArenaList::Segments { .. } => {}
        }

        let singleton_idx = self.push_list(vec![value]);
        self.push_list_concat(list, NanValue::new_list(singleton_idx))
    }

    pub fn push_list_concat(&mut self, left: NanValue, right: NanValue) -> u32 {
        debug_assert!(left.is_list());
        debug_assert!(right.is_list());
        if self.list_is_empty_value(left) {
            return if right.is_empty_list_immediate() {
                self.push_list(Vec::new())
            } else {
                right.arena_index()
            };
        }
        if self.list_is_empty_value(right) {
            return left.arena_index();
        }
        let len = self.list_len_value(left) + self.list_len_value(right);
        self.push(ArenaEntry::List(ArenaList::Concat { left, right, len }))
    }

    pub fn list_len_value(&self, list: NanValue) -> usize {
        if list.is_empty_list_immediate() {
            0
        } else {
            self.list_len(list.arena_index())
        }
    }

    pub fn list_is_empty_value(&self, list: NanValue) -> bool {
        self.list_len_value(list) == 0
    }

    pub fn list_get_value(&self, list: NanValue, position: usize) -> Option<NanValue> {
        if list.is_empty_list_immediate() {
            None
        } else {
            self.list_get(list.arena_index(), position)
        }
    }

    pub fn list_to_vec_value(&self, list: NanValue) -> Vec<NanValue> {
        if list.is_empty_list_immediate() {
            Vec::new()
        } else {
            self.list_to_vec(list.arena_index())
        }
    }

    pub fn list_len(&self, index: u32) -> usize {
        match self.get_list(index) {
            ArenaList::Flat { items, start } => items.len().saturating_sub(*start),
            ArenaList::Prepend { len, .. }
            | ArenaList::Concat { len, .. }
            | ArenaList::Segments { len, .. } => *len,
        }
    }

    pub fn list_is_empty(&self, index: u32) -> bool {
        self.list_len(index) == 0
    }

    pub fn list_get(&self, index: u32, position: usize) -> Option<NanValue> {
        let mut current = NanValue::new_list(index);
        let mut remaining = position;

        loop {
            if current.is_empty_list_immediate() {
                return None;
            }
            match self.get_list(current.arena_index()) {
                ArenaList::Flat { items, start } => {
                    return items.get(start.saturating_add(remaining)).copied();
                }
                ArenaList::Prepend { head, tail, .. } => {
                    if remaining == 0 {
                        return Some(*head);
                    }
                    remaining -= 1;
                    current = *tail;
                }
                ArenaList::Concat { left, right, .. } => {
                    let left_len = self.list_len_value(*left);
                    if remaining < left_len {
                        current = *left;
                    } else {
                        remaining -= left_len;
                        current = *right;
                    }
                }
                ArenaList::Segments {
                    current: head,
                    rest,
                    start,
                    ..
                } => {
                    let head_len = self.list_len_value(*head);
                    if remaining < head_len {
                        current = *head;
                    } else {
                        remaining -= head_len;
                        let mut found = None;
                        for part in &rest[*start..] {
                            let part_len = self.list_len_value(*part);
                            if remaining < part_len {
                                found = Some(*part);
                                break;
                            }
                            remaining -= part_len;
                        }
                        current = found?;
                    }
                }
            }
        }
    }

    pub fn list_to_vec(&self, index: u32) -> Vec<NanValue> {
        let mut out = Vec::with_capacity(self.list_len(index));
        let mut stack = vec![NanValue::new_list(index)];

        while let Some(list) = stack.pop() {
            if list.is_empty_list_immediate() {
                continue;
            }
            match self.get_list(list.arena_index()) {
                ArenaList::Flat { items, start } => {
                    out.extend(items[*start..].iter().copied());
                }
                ArenaList::Prepend { head, tail, .. } => {
                    out.push(*head);
                    stack.push(*tail);
                }
                ArenaList::Concat { left, right, .. } => {
                    stack.push(*right);
                    stack.push(*left);
                }
                ArenaList::Segments {
                    current,
                    rest,
                    start,
                    ..
                } => {
                    for part in rest[*start..].iter().rev() {
                        stack.push(*part);
                    }
                    stack.push(*current);
                }
            }
        }

        out
    }

    pub fn list_uncons(&mut self, list: NanValue) -> Option<(NanValue, NanValue)> {
        debug_assert!(list.is_list());
        if list.is_empty_list_immediate() {
            return None;
        }
        let mut rights = Vec::new();
        let mut current = list;

        loop {
            if current.is_empty_list_immediate() {
                return None;
            }
            match self.get_list(current.arena_index()).clone() {
                ArenaList::Flat { items, start } => {
                    let head = *items.get(start)?;
                    let tail = if start + 1 >= items.len() {
                        self.empty_list_value()
                    } else {
                        let tail_idx = self.push(ArenaEntry::List(ArenaList::Flat {
                            items: Rc::clone(&items),
                            start: start + 1,
                        }));
                        NanValue::new_list(tail_idx)
                    };
                    rights.reverse();
                    return Some((head, self.push_list_segments(tail, rights)));
                }
                ArenaList::Prepend { head, tail, .. } => {
                    rights.reverse();
                    return Some((head, self.push_list_segments(tail, rights)));
                }
                ArenaList::Concat { left, right, .. } => {
                    if self.list_is_empty_value(left) {
                        current = right;
                    } else {
                        rights.push(right);
                        current = left;
                    }
                }
                ArenaList::Segments {
                    current: head_segment,
                    rest,
                    start,
                    ..
                } => {
                    let (head, tail) = self.list_uncons(head_segment)?;
                    return Some((
                        head,
                        self.push_list_segments_rc(tail, Rc::clone(&rest), start),
                    ));
                }
            }
        }
    }

    fn empty_list_value(&mut self) -> NanValue {
        NanValue::EMPTY_LIST
    }

    fn push_list_segments(&mut self, current: NanValue, rest: Vec<NanValue>) -> NanValue {
        let filtered: Vec<NanValue> = rest
            .into_iter()
            .filter(|part| !self.list_is_empty_value(*part))
            .collect();
        self.push_list_segments_rc(current, Rc::new(filtered), 0)
    }

    fn push_list_segments_rc(
        &mut self,
        mut current: NanValue,
        rest: Rc<Vec<NanValue>>,
        mut start: usize,
    ) -> NanValue {
        while self.list_is_empty_value(current) {
            if let Some(next) = rest.get(start).copied() {
                current = next;
                start += 1;
            } else {
                return self.empty_list_value();
            }
        }

        if start >= rest.len() {
            return current;
        }

        let len = self.list_len_value(current)
            + rest[start..]
                .iter()
                .map(|part| self.list_len_value(*part))
                .sum::<usize>();
        let idx = self.push(ArenaEntry::List(ArenaList::Segments {
            current,
            rest,
            start,
            len,
        }));
        NanValue::new_list(idx)
    }
}
