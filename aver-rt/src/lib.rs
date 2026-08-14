#![doc = include_str!("../README.md")]

pub mod crypto;
mod display;
#[cfg(feature = "http")]
pub mod http;
pub mod http_server;
pub mod int;
#[cfg(feature = "random")]
pub mod random;
mod runtime;
mod service_types;
pub mod tcp;
#[cfg(feature = "terminal")]
pub mod terminal;

pub use display::{AverDisplay, aver_display};
pub use int::{AverInt, ShiftCountError};
pub use runtime::{
    append_text, cli_args, console_error, console_print, console_warn, delete_dir, delete_file,
    env_get, env_set, list_dir, make_dir, path_exists, read_line, read_text, string_slice,
    time_now, time_sleep, time_unix_ms, write_text,
};
pub use service_types::{
    BranchPath, HttpHeaders, HttpRequest, HttpResponse, TcpConnection, TerminalSize,
};

#[cfg(feature = "terminal")]
pub use terminal::{
    TerminalGuard, clear as terminal_clear, disable_raw_mode as terminal_disable_raw_mode,
    enable_raw_mode as terminal_enable_raw_mode, flush as terminal_flush,
    hide_cursor as terminal_hide_cursor, move_to as terminal_move_to,
    print_at_cursor as terminal_print, read_key as terminal_read_key,
    reset_color as terminal_reset_color, restore_terminal, set_color as terminal_set_color,
    show_cursor as terminal_show_cursor, size as terminal_size,
};

use std::collections::HashMap as StdHashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FusedIterator;

/// Internal builder for the deforestation lowering (0.15 Traversal).
/// Backs `__buf_*` intrinsics emitted when the compiler fuses
/// `String.join(<builder>(...), sep)` shapes — `String::with_capacity`
/// plus `push_str` is exactly the right shape, no GC dance needed.
/// User code never sees this directly; it lives strictly between
/// `__buf_new` and `__buf_finalize` inside synthesized helpers.
pub type Buffer = String;

/// Aver string type: newtype over Rc<str> for O(1) clone and native `+` operator.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct AverStr(Rc<str>);

impl AverStr {
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl std::ops::Deref for AverStr {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for AverStr {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for AverStr {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<String> for AverStr {
    fn from(s: String) -> Self {
        Self(Rc::from(s.as_str()))
    }
}

impl From<&str> for AverStr {
    fn from(s: &str) -> Self {
        Self(Rc::from(s))
    }
}

impl From<Rc<str>> for AverStr {
    fn from(s: Rc<str>) -> Self {
        Self(s)
    }
}

impl fmt::Display for AverStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for AverStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &*self.0)
    }
}

impl std::ops::Add<&AverStr> for AverStr {
    type Output = AverStr;
    fn add(self, other: &AverStr) -> AverStr {
        let mut s = String::with_capacity(self.len() + other.len());
        s.push_str(&self);
        s.push_str(other);
        AverStr::from(s)
    }
}

/// Concatenate two AverStr values.
#[inline]
pub fn aver_str_concat(a: &AverStr, b: &AverStr) -> AverStr {
    let mut s = String::with_capacity(a.len() + b.len());
    s.push_str(a);
    s.push_str(b);
    AverStr::from(s)
}
use std::sync::Arc as Rc;

// ── par_execute: parallel execution for independent products (?!) ─────────────────

/// Execute tasks in parallel using scoped threads.
/// All branches run to completion (`complete` mode).
pub fn par_execute<T: Send>(tasks: Vec<Box<dyn FnOnce() -> T + Send>>) -> Vec<T> {
    std::thread::scope(|s| {
        let handles: Vec<_> = tasks.into_iter().map(|task| s.spawn(task)).collect();
        handles.into_iter().map(|h| h.join().unwrap()).collect()
    })
}

/// Execute tasks sequentially, left-to-right. Used by `sequential` mode.
/// Valid per the language spec (the fully-sequential interleave is always
/// permitted). Available on every target — no threading required.
///
/// Signature matches [`par_execute`] (with `Send` bounds) so call sites
/// can swap implementations without reshaping task boxes.
pub fn par_execute_sequential<T: Send>(tasks: Vec<Box<dyn FnOnce() -> T + Send>>) -> Vec<T> {
    tasks.into_iter().map(|task| task()).collect()
}

/// Execute tasks in parallel with cooperative cancellation (`cancel` mode).
///
/// Each task receives a shared `cancelled` flag. When one branch fails, the
/// flag is set so siblings can check it and bail early. Tasks must call
/// `cancelled.load(Ordering::Relaxed)` at effect boundaries to cooperate.
/// Cancellable task: receives a shared cancellation flag, returns Result.
pub type CancelTask<T, E> =
    Box<dyn FnOnce(std::sync::Arc<std::sync::atomic::AtomicBool>) -> Result<T, E> + Send>;

pub fn par_execute_with_cancel<T: Send, E: Send>(
    tasks: Vec<CancelTask<T, E>>,
) -> Vec<Result<T, E>> {
    use std::sync::{Arc, atomic::AtomicBool};
    let cancelled = Arc::new(AtomicBool::new(false));
    std::thread::scope(|s| {
        let handles: Vec<_> = tasks
            .into_iter()
            .map(|task| {
                let flag = Arc::clone(&cancelled);
                s.spawn(move || {
                    let result = task(Arc::clone(&flag));
                    if result.is_err() {
                        flag.store(true, std::sync::atomic::Ordering::Relaxed);
                    }
                    result
                })
            })
            .collect();
        handles.into_iter().map(|h| h.join().unwrap()).collect()
    })
}

// ── AverMap: Copy-on-Write HashMap ──────────────────────────────────────────
//
// Semantically immutable (like im::HashMap), but when the Rc has a single
// owner we mutate in place — turning O(log n) persistent-set into O(1)
// amortized insert.

pub struct AverMap<K, V> {
    inner: Rc<StdHashMap<K, V>>,
}

impl<K, V> Clone for AverMap<K, V> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<K, V> AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub fn new() -> Self {
        Self {
            inner: Rc::new(StdHashMap::new()),
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.inner.contains_key(key)
    }

    /// O(n) because `&self` preserves the original map.
    pub fn insert(&self, key: K, value: V) -> Self {
        self.clone().insert_owned(key, value)
    }

    /// Identity of the backing table.
    ///
    /// Two maps that report the same value share one table; a value that
    /// changes across an update means `Rc::make_mut` found a second owner and
    /// rebuilt the table, duplicating every entry it held. That is what lets a
    /// caller count copy-on-write duplication exactly, instead of inferring it
    /// from which method it believes it called. The address cannot be reused
    /// while the old table is still alive, and `insert` keeps it alive across
    /// the clone, so a duplication can never be missed.
    pub fn table_id(&self) -> usize {
        Rc::as_ptr(&self.inner) as usize
    }

    /// O(1) amortized if unique owner, O(n) clone if shared.
    pub fn insert_owned(mut self, key: K, value: V) -> Self {
        Rc::make_mut(&mut self.inner).insert(key, value);
        self
    }

    /// Rewrite values in place using `Rc::make_mut` (zero-copy when sole owner).
    pub fn rewrite_values_in_place(&mut self, mut f: impl FnMut(&mut V)) {
        let inner = Rc::make_mut(&mut self.inner);
        for value in inner.values_mut() {
            f(value);
        }
    }

    /// O(n) because `&self` preserves the original map.
    pub fn remove(&self, key: &K) -> Self {
        self.clone().remove_owned(key)
    }

    /// O(1) amortized if unique owner, O(n) clone if shared.
    pub fn remove_owned(mut self, key: &K) -> Self {
        Rc::make_mut(&mut self.inner).remove(key);
        self
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.inner.keys()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.inner.values()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner.iter()
    }
}

impl<K, V> Default for AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone + PartialEq, V: PartialEq + Clone> PartialEq for AverMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<K: Eq + Hash + Clone, V: Eq + Clone> Eq for AverMap<K, V> {}

impl<K: Eq + Hash + Clone + Hash + Ord, V: Hash + Clone> Hash for AverMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Deterministic: sort keys for stable hash
        let mut keys: Vec<&K> = self.inner.keys().collect();
        keys.sort();
        keys.len().hash(state);
        for k in keys {
            k.hash(state);
            self.inner[k].hash(state);
        }
    }
}

impl<K: fmt::Debug + Eq + Hash + Clone, V: fmt::Debug + Clone> fmt::Debug for AverMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<K, V> std::ops::Index<&K> for AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    type Output = V;
    fn index(&self, key: &K) -> &V {
        &self.inner[key]
    }
}

impl<K, V> FromIterator<(K, V)> for AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        Self {
            inner: Rc::new(iter.into_iter().collect()),
        }
    }
}

// ── AverVector: COW indexed sequence ─────────────────────────────────────────

pub struct AverVector<T> {
    inner: Rc<Vec<T>>,
}

impl<T> Clone for AverVector<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<T: Clone> AverVector<T> {
    pub fn new(size: usize, default: T) -> Self {
        Self {
            inner: Rc::new(vec![default; size]),
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(index)
    }

    /// O(1) amortized if unique owner, O(n) clone if shared.
    ///
    /// Caller must ensure `index < len()`.
    pub fn set_unchecked(mut self, index: usize, value: T) -> Self {
        debug_assert!(index < self.inner.len());
        Rc::make_mut(&mut self.inner)[index] = value;
        self
    }

    /// O(1) amortized if unique owner, O(n) clone if shared. None if out of bounds.
    pub fn set_owned(self, index: usize, value: T) -> Option<Self> {
        if index >= self.inner.len() {
            return None;
        }
        Some(self.set_unchecked(index, value))
    }

    /// O(n) because `&self` preserves the original vector.
    pub fn set(&self, index: usize, value: T) -> Option<Self> {
        self.clone().set_owned(index, value)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn from_vec(v: Vec<T>) -> Self {
        Self { inner: Rc::new(v) }
    }

    pub fn to_vec(&self) -> Vec<T> {
        self.inner.as_ref().clone()
    }

    /// O(1) — shares Rc<Vec<T>> with the resulting Flat AverList.
    pub fn to_list(&self) -> AverList<T> {
        AverList::from_rc_vec(Rc::clone(&self.inner))
    }

    /// O(1) if list is Flat with start=0 (e.g. after List.reverse), O(n) otherwise.
    pub fn from_list(list: &AverList<T>) -> Self
    where
        T: Clone,
    {
        Self {
            inner: list.into_rc_vec(),
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.inner.iter()
    }
}

impl<T: PartialEq> PartialEq for AverVector<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T: Eq> Eq for AverVector<T> {}

impl<T: Hash> Hash for AverVector<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        9u8.hash(state);
        self.inner.hash(state);
    }
}

impl<T: fmt::Debug> fmt::Debug for AverVector<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vector")?;
        f.debug_list().entries(self.inner.iter()).finish()
    }
}

// ── AverList ─────────────────────────────────────────────────────────────────

const LIST_APPEND_CHUNK_LIMIT: usize = 128;

pub struct AverList<T> {
    inner: Rc<AverListInner<T>>,
}

enum AverListInner<T> {
    Flat {
        items: Rc<Vec<T>>,
        start: usize,
    },
    Prepend {
        head: T,
        tail: AverList<T>,
        len: usize,
    },
    Concat {
        left: AverList<T>,
        right: AverList<T>,
        len: usize,
    },
    Segments {
        current: AverList<T>,
        rest: Rc<Vec<AverList<T>>>,
        start: usize,
        len: usize,
    },
}

fn empty_list_inner<T>() -> Rc<AverListInner<T>> {
    Rc::new(AverListInner::Flat {
        items: Rc::new(Vec::new()),
        start: 0,
    })
}

fn empty_list<T>(inner: &Rc<AverListInner<T>>) -> AverList<T> {
    AverList {
        inner: Rc::clone(inner),
    }
}

fn take_list_inner<T>(
    list: &mut AverList<T>,
    empty_inner: &Rc<AverListInner<T>>,
) -> Rc<AverListInner<T>> {
    let original = std::mem::replace(list, empty_list(empty_inner));
    original.inner
}

fn detach_unique_children<T>(
    inner: &mut AverListInner<T>,
    empty_inner: &Rc<AverListInner<T>>,
    pending: &mut Vec<Rc<AverListInner<T>>>,
) {
    match inner {
        AverListInner::Flat { .. } => {}
        AverListInner::Prepend { tail, .. } => {
            pending.push(take_list_inner(tail, empty_inner));
        }
        AverListInner::Concat { left, right, .. } => {
            pending.push(take_list_inner(left, empty_inner));
            pending.push(take_list_inner(right, empty_inner));
        }
        AverListInner::Segments { current, rest, .. } => {
            pending.push(take_list_inner(current, empty_inner));
            let rest_rc = std::mem::replace(rest, Rc::new(Vec::new()));
            if let Ok(mut rest_vec) = Rc::try_unwrap(rest_rc) {
                for part in &mut rest_vec {
                    pending.push(take_list_inner(part, empty_inner));
                }
            }
        }
    }
}

impl<T> Drop for AverListInner<T> {
    fn drop(&mut self) {
        if matches!(self, AverListInner::Flat { .. }) {
            return;
        }

        let empty_inner = empty_list_inner();
        let mut pending = Vec::new();

        // Detach unique children eagerly so deep list teardown does not recurse
        // through nested `Rc<AverListInner<_>>` chains on the Rust call stack.
        detach_unique_children(self, &empty_inner, &mut pending);

        while let Some(child) = pending.pop() {
            if let Ok(mut child_inner) = Rc::try_unwrap(child) {
                detach_unique_children(&mut child_inner, &empty_inner, &mut pending);
            }
        }
    }
}

#[derive(Clone)]
enum ListCursor<'a, T> {
    Node(&'a AverList<T>),
    Slice(&'a [T], usize),
    SegmentSlice(&'a [AverList<T>], usize),
}

pub struct AverListIter<'a, T> {
    stack: Vec<ListCursor<'a, T>>,
    remaining: usize,
}

impl<T> Clone for AverList<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<T> AverList<T> {
    fn concat_node(left: &Self, right: &Self) -> Self {
        Self {
            inner: Rc::new(AverListInner::Concat {
                left: left.clone(),
                right: right.clone(),
                len: left.len() + right.len(),
            }),
        }
    }

    fn segments_rc(mut current: Self, rest: Rc<Vec<Self>>, mut start: usize) -> Self {
        while current.is_empty() {
            if let Some(next) = rest.get(start).cloned() {
                current = next;
                start += 1;
            } else {
                return Self::empty();
            }
        }

        if start >= rest.len() {
            return current;
        }

        let len = current.len() + rest[start..].iter().map(AverList::len).sum::<usize>();
        Self {
            inner: Rc::new(AverListInner::Segments {
                current,
                rest,
                start,
                len,
            }),
        }
    }

    fn rebuild_from_rights(mut base: Self, mut rights: Vec<Self>) -> Self {
        while let Some(right) = rights.pop() {
            base = Self::concat(&base, &right);
        }
        base
    }

    fn flat_tail(items: &Rc<Vec<T>>, start: usize) -> Option<Self> {
        if start >= items.len() {
            return None;
        }
        if start + 1 >= items.len() {
            return Some(Self::empty());
        }
        Some(Self {
            inner: Rc::new(AverListInner::Flat {
                items: Rc::clone(items),
                start: start + 1,
            }),
        })
    }

    fn uncons(&self) -> Option<(&T, Self)> {
        let mut rights = Vec::new();
        let mut current = self;

        loop {
            match current.inner.as_ref() {
                AverListInner::Flat { items, start } => {
                    let head = items.get(*start)?;
                    let tail = Self::flat_tail(items, *start)?;
                    return Some((head, Self::rebuild_from_rights(tail, rights)));
                }
                AverListInner::Prepend { head, tail, .. } => {
                    return Some((head, Self::rebuild_from_rights(tail.clone(), rights)));
                }
                AverListInner::Concat { left, right, .. } => {
                    if left.is_empty() {
                        current = right;
                        continue;
                    }
                    rights.push(right.clone());
                    current = left;
                }
                AverListInner::Segments {
                    current: head_segment,
                    rest,
                    start,
                    ..
                } => {
                    let (head, tail) = head_segment.uncons()?;
                    return Some((head, Self::segments_rc(tail, Rc::clone(rest), *start)));
                }
            }
        }
    }

    pub fn uncons_cloned(&self) -> Option<(T, Self)>
    where
        T: Clone,
    {
        self.uncons().map(|(head, tail)| (head.clone(), tail))
    }

    pub fn empty() -> Self {
        Self::from_vec(vec![])
    }

    pub fn from_vec(items: Vec<T>) -> Self {
        Self {
            inner: Rc::new(AverListInner::Flat {
                items: Rc::new(items),
                start: 0,
            }),
        }
    }

    /// O(1) if Flat with start=0, wraps existing Rc<Vec<T>> directly.
    pub fn from_rc_vec(items: Rc<Vec<T>>) -> Self {
        Self {
            inner: Rc::new(AverListInner::Flat { items, start: 0 }),
        }
    }

    /// Extract the backing Rc<Vec<T>> — O(1) if Flat with start=0, O(n) otherwise.
    pub fn into_rc_vec(&self) -> Rc<Vec<T>>
    where
        T: Clone,
    {
        match self.inner.as_ref() {
            AverListInner::Flat { items, start } if *start == 0 => Rc::clone(items),
            _ => Rc::new(self.to_vec()),
        }
    }

    pub fn len(&self) -> usize {
        match self.inner.as_ref() {
            AverListInner::Flat { items, start } => items.len().saturating_sub(*start),
            AverListInner::Prepend { len, .. }
            | AverListInner::Concat { len, .. }
            | AverListInner::Segments { len, .. } => *len,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        let mut current = self;
        let mut remaining = index;

        loop {
            match current.inner.as_ref() {
                AverListInner::Flat { items, start } => {
                    return items.get(start.saturating_add(remaining));
                }
                AverListInner::Prepend { head, tail, .. } => {
                    if remaining == 0 {
                        return Some(head);
                    }
                    remaining -= 1;
                    current = tail;
                }
                AverListInner::Concat { left, right, .. } => {
                    let left_len = left.len();
                    if remaining < left_len {
                        current = left;
                    } else {
                        remaining -= left_len;
                        current = right;
                    }
                }
                AverListInner::Segments {
                    current: head_segment,
                    rest,
                    start,
                    ..
                } => {
                    let head_len = head_segment.len();
                    if remaining < head_len {
                        current = head_segment;
                    } else {
                        remaining -= head_len;
                        let mut found = None;
                        for part in &rest[*start..] {
                            let part_len = part.len();
                            if remaining < part_len {
                                found = Some(part);
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

    pub fn first(&self) -> Option<&T> {
        self.get(0)
    }

    pub fn as_slice(&self) -> Option<&[T]> {
        match self.inner.as_ref() {
            AverListInner::Flat { items, start } => Some(items.get(*start..).unwrap_or(&[])),
            AverListInner::Prepend { .. }
            | AverListInner::Concat { .. }
            | AverListInner::Segments { .. } => None,
        }
    }

    pub fn iter(&self) -> AverListIter<'_, T> {
        AverListIter {
            stack: vec![ListCursor::Node(self)],
            remaining: self.len(),
        }
    }

    pub fn tail(&self) -> Option<Self> {
        match self.inner.as_ref() {
            AverListInner::Flat { items, start } => Self::flat_tail(items, *start),
            AverListInner::Prepend { tail, .. } => Some(tail.clone()),
            AverListInner::Concat { .. } | AverListInner::Segments { .. } => {
                self.uncons().map(|(_, tail)| tail)
            }
        }
    }

    pub fn prepend(item: T, list: &Self) -> Self {
        if list.is_empty() {
            return Self::from_vec(vec![item]);
        }
        Self {
            inner: Rc::new(AverListInner::Prepend {
                head: item,
                tail: list.clone(),
                len: list.len() + 1,
            }),
        }
    }

    pub fn concat(left: &Self, right: &Self) -> Self {
        if left.is_empty() {
            return right.clone();
        }
        if right.is_empty() {
            return left.clone();
        }
        Self::concat_node(left, right)
    }

    pub fn append(list: &Self, item: T) -> Self {
        let singleton = Self::from_vec(vec![item]);
        if list.is_empty() {
            return singleton;
        }

        match list.inner.as_ref() {
            AverListInner::Segments {
                current,
                rest,
                start,
                ..
            } => {
                let mut parts = rest[*start..].to_vec();
                if let Some(last) = parts.last_mut() {
                    if last.len() < LIST_APPEND_CHUNK_LIMIT {
                        *last = Self::concat(last, &singleton);
                    } else {
                        parts.push(singleton);
                    }
                } else {
                    parts.push(singleton);
                }
                Self::segments_rc(current.clone(), Rc::new(parts), 0)
            }
            _ if list.len() < LIST_APPEND_CHUNK_LIMIT => Self::concat(list, &singleton),
            _ => Self::segments_rc(list.clone(), Rc::new(vec![singleton]), 0),
        }
    }

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let mut out = Vec::with_capacity(self.len());
        out.extend(self.iter().cloned());
        out
    }

    pub fn reverse(&self) -> Self
    where
        T: Clone,
    {
        let mut out = self.to_vec();
        out.reverse();
        Self::from_vec(out)
    }

    pub fn contains(&self, item: &T) -> bool
    where
        T: PartialEq,
    {
        self.iter().any(|x| x == item)
    }
}

impl<'a, T> Iterator for AverListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(cursor) = self.stack.pop() {
            match cursor {
                ListCursor::Slice(items, index) => {
                    if let Some(item) = items.get(index) {
                        self.stack.push(ListCursor::Slice(items, index + 1));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(item);
                    }
                }
                ListCursor::Node(list) => match list.inner.as_ref() {
                    AverListInner::Flat { items, start } => {
                        let slice = items.get(*start..).unwrap_or(&[]);
                        if !slice.is_empty() {
                            self.stack.push(ListCursor::Slice(slice, 0));
                        }
                    }
                    AverListInner::Prepend { head, tail, .. } => {
                        self.stack.push(ListCursor::Node(tail));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(head);
                    }
                    AverListInner::Concat { left, right, .. } => {
                        self.stack.push(ListCursor::Node(right));
                        self.stack.push(ListCursor::Node(left));
                    }
                    AverListInner::Segments {
                        current,
                        rest,
                        start,
                        ..
                    } => {
                        let slice = rest.get(*start..).unwrap_or(&[]);
                        if !slice.is_empty() {
                            self.stack.push(ListCursor::SegmentSlice(slice, 0));
                        }
                        self.stack.push(ListCursor::Node(current));
                    }
                },
                ListCursor::SegmentSlice(items, index) => {
                    if let Some(item) = items.get(index) {
                        self.stack.push(ListCursor::SegmentSlice(items, index + 1));
                        self.stack.push(ListCursor::Node(item));
                    }
                }
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<T> ExactSizeIterator for AverListIter<'_, T> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl<T> FusedIterator for AverListIter<'_, T> {}

impl<'a, T> IntoIterator for &'a AverList<T> {
    type Item = &'a T;
    type IntoIter = AverListIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T: Clone> IntoIterator for AverList<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}

impl<T: fmt::Debug> fmt::Debug for AverList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: PartialEq> PartialEq for AverList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}

impl<T: Eq> Eq for AverList<T> {}

impl<T: Hash> Hash for AverList<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        8u8.hash(state);
        self.len().hash(state);
        for item in self.iter() {
            item.hash(state);
        }
    }
}

pub fn list_uncons<T>(list: &AverList<T>) -> Option<(&T, AverList<T>)> {
    list.uncons()
}

pub fn list_uncons_cloned<T: Clone>(list: &AverList<T>) -> Option<(T, AverList<T>)> {
    list.uncons_cloned()
}

/// Pattern-match on an AverList: empty and cons (head, tail) arms.
#[macro_export]
macro_rules! aver_list_match {
    ($list:expr, [] => $empty:expr, [$head:ident, $tail:ident] => $cons:expr) => {{
        let __aver_list = $list;
        if __aver_list.is_empty() {
            $empty
        } else if let ::core::option::Option::Some(($head, $tail)) =
            $crate::list_uncons_cloned(&__aver_list)
        {
            $cons
        } else {
            panic!("Aver: non-exhaustive list match")
        }
    }};
}

pub fn string_join<S: AsRef<str>>(parts: &AverList<S>, sep: &str) -> String {
    let mut iter = parts.iter();
    let Some(first) = iter.next() else {
        return String::new();
    };
    let mut out = first.as_ref().to_string();
    for part in iter {
        out.push_str(sep);
        out.push_str(part.as_ref());
    }
    out
}

#[cfg(test)]
mod tests {
    use super::{
        AverList, AverListInner, LIST_APPEND_CHUNK_LIMIT, aver_display, env_set, string_slice,
    };

    #[test]
    fn prepend_and_tail_share_structure() {
        let base = AverList::from_vec(vec![2, 3]);
        let full = AverList::prepend(1, &base);
        assert_eq!(full.first(), Some(&1));
        assert_eq!(full.tail().unwrap(), base);
    }

    #[test]
    fn concat_and_iter_preserve_order() {
        let left = AverList::from_vec(vec![1, 2]);
        let right = AverList::from_vec(vec![3, 4]);
        let joined = AverList::concat(&left, &right);
        assert_eq!(joined.to_vec(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn dropping_deep_prepend_chain_does_not_overflow() {
        let mut list = AverList::empty();
        for value in 0..200_000 {
            list = AverList::prepend(value, &list);
        }

        assert_eq!(list.len(), 200_000);
        drop(list);
    }

    #[test]
    fn tail_of_deep_append_chain_does_not_overflow() {
        let mut list = AverList::empty();
        for value in 0..200_000 {
            list = AverList::append(&list, value);
        }

        let tail = list.tail().expect("non-empty list must have a tail");
        assert_eq!(tail.len(), 199_999);
        assert_eq!(tail.first(), Some(&1));
    }

    #[test]
    fn list_uncons_of_deep_append_chain_does_not_overflow() {
        let mut list = AverList::empty();
        for value in 0..200_000 {
            list = AverList::append(&list, value);
        }

        let (head, tail) = super::list_uncons(&list).expect("non-empty list must uncons");
        assert_eq!(*head, 0);
        assert_eq!(tail.len(), 199_999);
        assert_eq!(tail.first(), Some(&1));
    }

    #[test]
    fn cloned_uncons_preserves_append_chain_tail_contents() {
        let mut list = AverList::empty();
        for value in 0..5 {
            list = AverList::append(&list, value);
        }

        let (head, tail) = super::list_uncons_cloned(&list).expect("non-empty list must uncons");
        assert_eq!(head, 0);
        assert_eq!(tail.to_vec(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn get_reads_flat_list_in_place() {
        let list = AverList::from_vec(vec![10, 20, 30]);

        assert_eq!(list.get(0), Some(&10));
        assert_eq!(list.get(2), Some(&30));
        assert_eq!(list.get(3), None);
    }

    #[test]
    fn get_walks_concat_and_prepend_without_flattening() {
        let base = AverList::from_vec(vec![2, 3]);
        let prepended = AverList::prepend(1, &base);
        let joined = AverList::concat(&prepended, &AverList::from_vec(vec![4, 5]));

        assert_eq!(joined.get(0), Some(&1));
        assert_eq!(joined.get(2), Some(&3));
        assert_eq!(joined.get(4), Some(&5));
        assert_eq!(joined.get(5), None);
    }

    #[test]
    fn repeated_tail_over_append_chain_preserves_all_items() {
        let mut list = AverList::empty();
        for value in 0..6 {
            list = AverList::append(&list, value);
        }

        let mut rest = list;
        let mut seen = Vec::new();
        while let Some((head, tail)) = super::list_uncons(&rest) {
            seen.push(*head);
            rest = tail;
        }

        assert_eq!(seen, vec![0, 1, 2, 3, 4, 5]);
    }

    #[test]
    fn append_promotes_long_right_spines_into_segments() {
        let mut list = AverList::empty();
        for value in 0..200 {
            list = AverList::append(&list, value);
        }

        match list.inner.as_ref() {
            AverListInner::Segments {
                current,
                rest,
                start,
                ..
            } => {
                assert_eq!(current.len(), LIST_APPEND_CHUNK_LIMIT);
                assert_eq!(rest[*start].len(), 72);
            }
            other => panic!(
                "expected segmented append shape, got {}",
                aver_display_shape(other)
            ),
        }
    }

    #[test]
    fn get_walks_segmented_append_chain_without_losing_order() {
        let mut list = AverList::empty();
        for value in 0..300 {
            list = AverList::append(&list, value);
        }

        assert_eq!(list.get(0), Some(&0));
        assert_eq!(list.get(127), Some(&127));
        assert_eq!(list.get(128), Some(&128));
        assert_eq!(list.get(255), Some(&255));
        assert_eq!(list.get(299), Some(&299));
        assert_eq!(list.get(300), None);
    }

    #[test]
    fn aver_display_quotes_strings_inside_lists() {
        let parts = AverList::from_vec(vec!["a".to_string(), "b".to_string()]);
        assert_eq!(aver_display(&parts), "[\"a\", \"b\"]");
    }

    #[test]
    fn string_slice_uses_code_point_indices() {
        assert_eq!(string_slice("zażółć", 1, 4), "ażó");
    }

    #[test]
    fn string_slice_clamps_negative_indices() {
        assert_eq!(string_slice("hello", -2, 2), "he");
        assert_eq!(string_slice("hello", 1, -1), "");
    }

    #[test]
    fn env_set_rejects_invalid_keys() {
        assert_eq!(
            env_set("", "x"),
            Err("Env.set: key must not be empty".to_string())
        );
        assert_eq!(
            env_set("A=B", "x"),
            Err("Env.set: key must not contain '='".to_string())
        );
    }

    fn aver_display_shape<T>(inner: &AverListInner<T>) -> &'static str {
        match inner {
            AverListInner::Flat { .. } => "Flat",
            AverListInner::Prepend { .. } => "Prepend",
            AverListInner::Concat { .. } => "Concat",
            AverListInner::Segments { .. } => "Segments",
        }
    }
}
