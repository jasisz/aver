use super::*;
use alloc::vec;

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
            ArenaList::Flat { items, start, .. } => {
                let slice = &items[start..];
                if slice.len() < LIST_APPEND_CHUNK_LIMIT {
                    let mut out = Vec::with_capacity(slice.len() + 1);
                    out.extend(slice.iter().copied());
                    out.push(value);
                    return self.push_list(out);
                }
            }
            ArenaList::Concat { left, right, len } => {
                if let ArenaList::Flat { items, start, .. } =
                    self.get_list(right.arena_index()).clone()
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
            ArenaList::Flat { items, start, .. } => items.len().saturating_sub(*start),
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
                ArenaList::Flat { items, start, .. } => {
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
                ArenaList::Flat { items, start, .. } => {
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

        self.note_list_elements_flattened(out.len());
        out
    }

    /// The first `count` elements, reading no more of the list than that.
    ///
    /// The bounded counterpart of [`Arena::list_to_vec`]: a prefix is answered
    /// by walking the prefix, so asking for a few elements of a long list costs
    /// what was asked for and not what the list holds. Flattening the list and
    /// throwing most of it away is what made `List.take` cost the whole list on
    /// every call, and a walk that steps with it quadratic.
    ///
    /// Only the spine down to the prefix is walked past it: a left-deep concat
    /// spine is descended to its leftmost element the way [`Arena::list_drop`]
    /// and [`Arena::list_get`] descend it.
    pub fn list_prefix(&self, list: NanValue, count: usize) -> Vec<NanValue> {
        debug_assert!(list.is_list());
        if count == 0 {
            return Vec::new();
        }
        let mut out = Vec::with_capacity(count.min(self.list_len_value(list)));
        let mut pending: Vec<NanValue> = Vec::new();
        let mut current = list;

        loop {
            if out.len() == count {
                return out;
            }
            if self.list_len_value(current) == 0 {
                match pending.pop() {
                    Some(next) => current = next,
                    None => return out,
                }
                continue;
            }
            match self.get_list(current.arena_index()) {
                ArenaList::Flat { items, start, .. } => {
                    let slice = &items[*start..];
                    let wanted = (count - out.len()).min(slice.len());
                    out.extend(slice[..wanted].iter().copied());
                    current = NanValue::EMPTY_LIST;
                }
                ArenaList::Prepend { head, tail, .. } => {
                    out.push(*head);
                    current = *tail;
                }
                ArenaList::Concat { left, right, .. } => {
                    pending.push(*right);
                    current = *left;
                }
                ArenaList::Segments {
                    current: head,
                    rest,
                    start,
                    ..
                } => {
                    // Only the segments the prefix can reach are queued: each
                    // one it does queue carries at least one element towards
                    // `count`, so a node holding many segments is not walked in
                    // full for a short prefix.
                    let mut reachable = 0usize;
                    let mut queued: Vec<NanValue> = Vec::new();
                    let wanted = count - out.len();
                    for part in &rest[*start..] {
                        if reachable >= wanted {
                            break;
                        }
                        reachable += self.list_len_value(*part);
                        queued.push(*part);
                    }
                    for part in queued.iter().rev() {
                        pending.push(*part);
                    }
                    current = *head;
                }
            }
        }
    }

    /// Keep the first `count` elements, reading no more of the list than that.
    ///
    /// The cost is what is kept, not what is left behind — the mirror of what
    /// [`Arena::list_drop`] does at the other end. A list shorter than the count
    /// is handed straight back rather than rebuilt, and a count of nothing is
    /// the canonical empty list, so the two edges cost nothing at all.
    pub fn list_take(&mut self, list: NanValue, count: usize) -> NanValue {
        debug_assert!(list.is_list());
        let len = self.list_len_value(list);
        if count == 0 || len == 0 {
            return NanValue::EMPTY_LIST;
        }
        if count >= len {
            return list;
        }
        let items = self.list_prefix(list, count);
        if items.is_empty() {
            return NanValue::EMPTY_LIST;
        }
        NanValue::new_list(self.push_list(items))
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
                ArenaList::Flat {
                    items,
                    start,
                    scan_receipt,
                } => {
                    let head = *items.get(start)?;
                    let tail = if start + 1 >= items.len() {
                        self.empty_list_value()
                    } else {
                        let tail_idx = self.push(ArenaEntry::List(ArenaList::Flat {
                            items: Rc::clone(&items),
                            start: start + 1,
                            scan_receipt,
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
                    if rights.is_empty() {
                        return Some((
                            head,
                            self.push_list_segments_rc(tail, Rc::clone(&rest), start),
                        ));
                    }
                    // This Segments node was reached down the left spine of a
                    // Concat, so right-siblings were accumulated in `rights`
                    // (in reverse order). They must follow this node's own
                    // remaining segments — the Flat/Prepend arms fold `rights`
                    // in the same way; omitting it here silently drops every
                    // element to the right of the Segments node.
                    let mut parts: Vec<NanValue> = rest[start..].to_vec();
                    rights.reverse();
                    parts.extend(rights);
                    return Some((head, self.push_list_segments(tail, parts)));
                }
            }
        }
    }

    /// Step over the first `count` elements, sharing what `list_uncons`
    /// shares.
    ///
    /// The cost is what is stepped over, not what is left: a flat body is
    /// handed back as a view over the same allocation with `start` advanced —
    /// the slice `list_uncons` already hands out — and every other shape is
    /// walked the way repeated `list_uncons` walks it. Rebuilding the
    /// remainder instead makes a walk over a list quadratic, which is what
    /// `List.drop` used to do (issue #913).
    ///
    /// Sharing the body keeps the stepped-over elements alive for as long as
    /// the view is, the same trade `list_uncons` already makes.
    pub fn list_drop(&mut self, list: NanValue, count: usize) -> NanValue {
        debug_assert!(list.is_list());
        let mut rights = Vec::new();
        let mut current = list;
        let mut remaining = count;

        loop {
            if remaining == 0 {
                rights.reverse();
                return self.push_list_segments(current, rights);
            }
            let len = self.list_len_value(current);
            if remaining >= len {
                // The whole node is stepped over; continue into whatever the
                // concat spine left waiting to its right.
                remaining -= len;
                match rights.pop() {
                    Some(next) => {
                        current = next;
                        continue;
                    }
                    None => return NanValue::EMPTY_LIST,
                }
            }

            // The step lands strictly inside `current`.
            match self.get_list(current.arena_index()).clone() {
                ArenaList::Flat {
                    items,
                    start,
                    scan_receipt,
                } => {
                    let view_idx = self.push(ArenaEntry::List(ArenaList::Flat {
                        items: Rc::clone(&items),
                        start: start + remaining,
                        scan_receipt,
                    }));
                    rights.reverse();
                    return self.push_list_segments(NanValue::new_list(view_idx), rights);
                }
                ArenaList::Prepend { tail, .. } => {
                    remaining -= 1;
                    current = tail;
                }
                ArenaList::Concat { left, right, .. } => {
                    rights.push(right);
                    current = left;
                }
                ArenaList::Segments {
                    current: head_segment,
                    rest,
                    start,
                    ..
                } => {
                    let mut segment = head_segment;
                    let mut index = start;
                    loop {
                        let segment_len = self.list_len_value(segment);
                        if remaining < segment_len {
                            break;
                        }
                        remaining -= segment_len;
                        match rest.get(index).copied() {
                            Some(next) => {
                                segment = next;
                                index += 1;
                            }
                            // Only reachable if a `len` field disagreed with
                            // the parts it counts; step over what is there.
                            None => {
                                segment = NanValue::EMPTY_LIST;
                                break;
                            }
                        }
                    }
                    let stepped = self.list_drop(segment, remaining);
                    if rights.is_empty() {
                        return self.push_list_segments_rc(stepped, Rc::clone(&rest), index);
                    }
                    // Same fold as `list_uncons`: the right-siblings collected
                    // down the concat spine follow this node's own remaining
                    // segments.
                    let mut parts: Vec<NanValue> = rest[index..].to_vec();
                    rights.reverse();
                    parts.extend(rights);
                    return self.push_list_segments(stepped, parts);
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
        self.push_list_segments_rc(current, Rc::new(ListBody::new(filtered)), 0)
    }

    fn push_list_segments_rc(
        &mut self,
        mut current: NanValue,
        rest: Rc<ListBody>,
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
