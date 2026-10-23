#![doc = include_str!("../README.md")]

pub mod provider;

pub mod bytebuilder;
pub mod crypto;
mod display;
#[cfg(feature = "http")]
pub mod http;
pub mod int;
pub mod int_list;
pub mod listbuilder;
#[cfg(all(not(target_family = "wasm"), any(feature = "mimalloc", test)))]
mod mimalloc;
pub mod packed_u8;
#[cfg(feature = "random")]
pub mod random;
mod runtime;
mod service_types;
mod shared;
mod shared_eq;
pub mod shared_hash;
pub mod strcursor;
mod string_index;
pub mod tcp;
#[cfg(feature = "terminal")]
pub mod terminal;
pub mod thin;
pub mod work;

pub use bytebuilder::{ByteBuilder, byte_builder_finalize, byte_builder_new, byte_builder_push};
pub use display::{AverDisplay, aver_display};
pub use int::{
    AverInt, MAX_MATERIALIZED_BITS, ShiftCountError, bit_width_too_large_message,
    int_endian_value_error_message, int_endian_width_error_message, int_from_big_endian,
    int_from_little_endian, int_to_big_endian, int_to_little_endian, shift_count_too_large_message,
};
pub use int_list::{
    AverIntList, AverIntListIter, int_list_builder_finalize, int_list_builder_new,
    int_list_builder_push, into_packed_u8,
};
pub use listbuilder::{list_builder_finalize, list_builder_new, list_builder_push};
#[cfg(all(not(target_family = "wasm"), feature = "mimalloc"))]
pub use mimalloc::MiMalloc;
pub use packed_u8::AverPackedU8;
pub use runtime::{
    append_bytes, append_text, capture_console_output, cli_args, console_error, console_print,
    console_warn, delete_dir, delete_file, env_get, env_set, file_size, list_dir, make_dir,
    path_exists, read_bytes, read_bytes_at, read_line, read_text, string_slice, sync_path,
    time_now, time_sleep, time_unix_ms, write_bytes, write_text,
};
pub use service_types::{
    BranchPath, HttpHeaders, HttpRequest, HttpResponse, TcpConnection, TcpDial, TcpListener,
    TerminalSize,
};
pub use shared::{Shared, StaticStr};
pub use shared_eq::{NodeId, Walk, WalkEq, walk_eq};
pub use shared_hash::{HashWalk, WalkHash};
pub use strcursor::{
    str_code1, str_code1_lower, str_code1_upper, str_cursor_code, str_cursor_end, str_cursor_head,
    str_cursor_next, str_fold_lower, str_fold_upper,
};
pub use string_index::{
    StringIndex, string_index_build, string_index_char_at, string_index_code_at, string_index_slice,
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

use ahash::RandomState as AHashRandomState;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FusedIterator;

/// Internal builder for the deforestation lowering (0.15 Traversal).
/// Backs `__buf_*` intrinsics emitted when the compiler fuses
/// `String.join(<builder>(...), sep)` shapes.
///
/// `has_parts` is intentionally distinct from `bytes.is_empty()`: joining
/// `["", "b"]` must emit `",b"`, so the separator decision records that an
/// empty first fragment was appended. User code never sees this type; it lives
/// strictly between `__buf_new` and `__buf_finalize`.
pub struct Buffer {
    bytes: String,
    has_parts: bool,
}

impl Buffer {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            bytes: String::with_capacity(capacity),
            has_parts: false,
        }
    }

    pub fn push_str(&mut self, value: &str) {
        self.bytes.push_str(value);
        self.has_parts = true;
    }

    /// Internal "is first fragment" predicate retained under the old method
    /// name so already-generated self-host code shares the corrected contract.
    pub fn is_empty(&self) -> bool {
        !self.has_parts
    }
}

/// Aver string type: newtype over `Shared<str>` for O(1) clone and native `+` operator.
///
/// A string literal points to a node in a static ([`str_lit!`]): cloning or
/// dropping it touches no count, so threads that share it never write to
/// one cache line.
#[derive(Eq, Ord, PartialOrd)]
pub struct AverStr(std::mem::ManuallyDrop<Rc<str>>);

impl Clone for AverStr {
    #[inline]
    fn clone(&self) -> Self {
        if Rc::is_static(&self.0) {
            // A static node is not counted, so a copy of the pointer owns it.
            Self(unsafe { std::ptr::read(&self.0) })
        } else {
            Self(self.0.clone())
        }
    }
}

impl Drop for AverStr {
    #[inline]
    fn drop(&mut self) {
        if !Rc::is_static(&self.0) {
            unsafe { std::mem::ManuallyDrop::drop(&mut self.0) }
        }
    }
}

/// The `AverStr` of a string literal, in a static: no allocation or copy
/// when it runs, and none when it is cloned or dropped.
#[macro_export]
macro_rules! str_lit {
    ($text:literal) => {{
        static NODE: $crate::StaticStr<{ $text.len() }> = $crate::StaticStr::new($text);
        $crate::AverStr::from_static(&NODE)
    }};
}

/// One string is equal to itself without reading it, as `Arc<str>`'s `==`.
impl PartialEq for AverStr {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl Hash for AverStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (*self.0).hash(state)
    }
}

impl AverStr {
    #[inline]
    fn new(text: Rc<str>) -> Self {
        Self(std::mem::ManuallyDrop::new(text))
    }
    /// The string a static holds; see [`str_lit!`].
    #[inline]
    pub fn from_static<const N: usize>(node: &'static StaticStr<N>) -> Self {
        Self::new(Rc::from_static(node))
    }
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
        Self::new(Rc::from(s.as_str()))
    }
}

impl From<Buffer> for AverStr {
    fn from(buffer: Buffer) -> Self {
        Self::new(Rc::from(buffer.bytes.as_str()))
    }
}

impl From<&str> for AverStr {
    fn from(s: &str) -> Self {
        Self::new(Rc::from(s))
    }
}

impl From<Rc<str>> for AverStr {
    fn from(s: Rc<str>) -> Self {
        Self::new(s)
    }
}

/// From hand-written code that holds an `Arc<str>`: the text is copied.
impl From<std::sync::Arc<str>> for AverStr {
    fn from(s: std::sync::Arc<str>) -> Self {
        Self::new(Rc::from(&*s))
    }
}

impl fmt::Display for AverStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for AverStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &**self.0)
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
/// Every node the runtime's values share (lists, vectors, maps, strings).
use crate::shared::Shared as Rc;

// ── par_execute: parallel execution for independent products (?!) ─────────────────

/// Stack size of every thread an Aver program runs on: `main`, the branches
/// of an independent product and `Work` jobs. A recursion that fits in
/// `main` must fit in a branch or a job too.
pub const THREAD_STACK_BYTES: usize = 256 * 1024 * 1024;

/// The process exit status of a `main` that returns `Int`: the value itself
/// when it lies in 0..=255, the range every platform reports. Any other value
/// is said on stderr and ends in status 1, so a truncated status never reads
/// as success. Every backend asks this one function.
pub fn main_exit_status(code: &AverInt) -> i32 {
    match code.to_i64() {
        Some(status @ 0..=255) => status as i32,
        _ => {
            eprintln!("main returned exit code {code}, outside 0..255");
            1
        }
    }
}

/// Spawn a scoped thread with [`THREAD_STACK_BYTES`] of stack instead of
/// Rust's default of 2 MB.
pub fn spawn_scoped<'scope, 'env, T: Send + 'scope>(
    scope: &'scope std::thread::Scope<'scope, 'env>,
    body: impl FnOnce() -> T + Send + 'scope,
) -> std::thread::ScopedJoinHandle<'scope, T> {
    std::thread::Builder::new()
        .stack_size(THREAD_STACK_BYTES)
        .spawn_scoped(scope, body)
        .expect("failed to spawn a thread")
}

/// Execute tasks in parallel using scoped threads.
/// All branches run to completion (`complete` mode).
pub fn par_execute<T: Send>(tasks: Vec<Box<dyn FnOnce() -> T + Send>>) -> Vec<T> {
    std::thread::scope(|s| {
        let handles: Vec<_> = tasks
            .into_iter()
            .map(|task| spawn_scoped(s, task))
            .collect();
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

/// Native backing table for an Aver `Map`.
///
/// Generated programs can put network-derived values into a map, so keep a
/// random key while avoiding SipHash13's cost on structural and byte keys.
/// The hash is internal only: every observable map walk is sorted.
type AverHashMap<K, V> = HashMap<K, V, AHashRandomState>;

/// The key every map hashes with: drawn once per process, so a new map only
/// copies it. Nothing builds one map by walking another in hash order (every
/// walk is sorted), so tables do not need keys of their own.
fn map_hash_state() -> AHashRandomState {
    static STATE: std::sync::OnceLock<AHashRandomState> = std::sync::OnceLock::new();
    STATE.get_or_init(AHashRandomState::new).clone()
}

/// How many entries a map's first table holds: a written map starts here
/// rather than growing from nothing through 3, 7 and 14 slots.
const MAP_FIRST_CAPACITY: usize = 16;

pub struct AverMap<K, V> {
    /// The table, or none for a map never written: `{}` allocates nothing,
    /// and its clones and drops touch no count.
    inner: Option<Rc<AverHashMap<K, V>>>,
}

impl<K, V> Clone for AverMap<K, V> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<K, V> AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub fn new() -> Self {
        Self { inner: None }
    }

    /// The table to write, made at the first write. Shared, it is copied.
    fn table_mut(&mut self) -> &mut AverHashMap<K, V> {
        Rc::make_mut(self.inner.get_or_insert_with(|| {
            Rc::new(AverHashMap::with_capacity_and_hasher(
                MAP_FIRST_CAPACITY,
                map_hash_state(),
            ))
        }))
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.as_ref()?.get(key)
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.inner.as_ref().is_some_and(|map| map.contains_key(key))
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
    /// the clone, so a duplication can never be missed. A map never written
    /// has no table and answers 0; its first write duplicates nothing.
    pub fn table_id(&self) -> usize {
        self.inner
            .as_ref()
            .map_or(0, |map| Rc::as_ptr(map) as usize)
    }

    /// O(1) amortized if unique owner, O(n) clone if shared.
    pub fn insert_owned(mut self, key: K, value: V) -> Self {
        self.table_mut().insert(key, value);
        self
    }

    /// Rewrite values in place using `Rc::make_mut` (zero-copy when sole owner).
    pub fn rewrite_values_in_place(&mut self, mut f: impl FnMut(&mut V)) {
        let Some(map) = &mut self.inner else {
            return;
        };
        for value in Rc::make_mut(map).values_mut() {
            f(value);
        }
    }

    /// Rewrite one value in place using `Rc::make_mut`.
    ///
    /// Returns `false` when the key is absent, so remembered-set consumers can
    /// safely discard a stale key left behind by a removal.
    pub fn rewrite_value_in_place(&mut self, key: &K, f: impl FnOnce(&mut V)) -> bool {
        let Some(map) = &mut self.inner else {
            return false;
        };
        let Some(value) = Rc::make_mut(map).get_mut(key) else {
            return false;
        };
        f(value);
        true
    }

    /// O(n) because `&self` preserves the original map.
    pub fn remove(&self, key: &K) -> Self {
        self.clone().remove_owned(key)
    }

    /// O(1) amortized if unique owner, O(n) clone if shared.
    pub fn remove_owned(mut self, key: &K) -> Self {
        if let Some(map) = &mut self.inner {
            Rc::make_mut(map).remove(key);
        }
        self
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.inner.iter().flat_map(|map| map.keys())
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.inner.iter().flat_map(|map| map.values())
    }

    pub fn len(&self) -> usize {
        self.inner.as_ref().map_or(0, |map| map.len())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner.iter().flat_map(|map| map.iter())
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
        match (&self.inner, &other.inner) {
            (Some(a), Some(b)) => a == b,
            // A map with no table is empty, as a table emptied by removals.
            _ => self.len() == other.len(),
        }
    }
}

impl<K: Eq + Hash + Clone, V: Eq + Clone> Eq for AverMap<K, V> {}

impl<K: Eq + Hash + Clone + Hash + Ord, V: Hash + Clone> Hash for AverMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Deterministic: sort keys for stable hash
        let mut entries: Vec<(&K, &V)> = self.iter().collect();
        entries.sort_by(|a, b| a.0.cmp(b.0));
        entries.len().hash(state);
        for (k, v) in entries {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl<K: fmt::Debug + Eq + Hash + Clone, V: fmt::Debug + Clone> fmt::Debug for AverMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K, V> std::ops::Index<&K> for AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    type Output = V;
    fn index(&self, key: &K) -> &V {
        self.get(key).expect("key not found")
    }
}

impl<K, V> FromIterator<(K, V)> for AverMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut inner = AverHashMap::with_hasher(map_hash_state());
        inner.extend(iter);
        Self {
            inner: (!inner.is_empty()).then(|| Rc::new(inner)),
        }
    }
}

// ── AverVector: COW indexed sequence ─────────────────────────────────────────

/// Maximum number of elements one `Vector.new` call may materialize.
///
/// This is an element budget, not a guessed byte estimate: Aver has no
/// portable `sizeof(T)`, and the VM, generated Rust, wasm GC, and Lean
/// intentionally use different representations for the same element type.
/// One mebielement still gives every backend one exact semantic boundary and
/// caps the operation at a finite number of clones/slots. On the VM it is
/// roughly 8 MiB of `NanValue` slots before arena overhead.
/// Portable element budget for a single operation that can expand a compact
/// input into a sequence. `Vector.new` and the fixed-width Int encoders share
/// the number, while retaining operation-specific diagnostics.
pub const MAX_MATERIALIZED_SEQUENCE_ELEMENTS: usize = 1024 * 1024;

pub const MAX_MATERIALIZED_VECTOR_ELEMENTS: usize = MAX_MATERIALIZED_SEQUENCE_ELEMENTS;

/// Convert an Aver integer to a materializable vector length.
///
/// `None` covers negative, arbitrary-precision, addressability, and policy
/// overflow uniformly. The policy bound is below every supported backend's
/// addressability ceiling, so callers cannot accidentally enforce the two in
/// the opposite order.
pub fn checked_vector_size(size: &AverInt) -> Option<usize> {
    size.to_usize()
        .filter(|&size| size <= MAX_MATERIALIZED_VECTOR_ELEMENTS)
}

/// Stable value-level error for a refused `Vector.new` materialization.
pub fn vector_size_error_message() -> String {
    format!("Vector.new: size must be between 0 and {MAX_MATERIALIZED_VECTOR_ELEMENTS}")
}

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
    /// The node, or none for the empty list: `[]` allocates nothing, and its
    /// clones and drops touch no count.
    inner: Option<Rc<AverListInner<T>>>,
    /// How many elements at the front of a shared flat or segmented body this
    /// list starts past.
    ///
    /// It lives on the list rather than in the shared node so that the rest of
    /// a flat list is the very same node read from one element further in —
    /// stepping is a reference count and an offset, with nothing built. Flat
    /// and indexed bodies carry an offset. The other shapes are whole nodes.
    start: usize,
    /// Segment containing `start`, when `inner` is `Indexed`.
    ///
    /// Keeping the cursor on the cheap list wrapper makes the next `uncons`
    /// O(1): a tail shares the segment table and advances these two integers.
    segment_index: usize,
}

struct TraversalSegment<T> {
    source: TraversalSource<T>,
    /// Number of visible elements in this constant-time segment.
    len: usize,
    /// Exclusive cumulative element offset in the segment table.
    end: usize,
}

enum TraversalSource<T> {
    Flat { items: Rc<Vec<T>>, start: usize },
    PrependHead { node: Rc<AverListInner<T>> },
}

impl<T> Clone for TraversalSegment<T> {
    fn clone(&self) -> Self {
        Self {
            source: match &self.source {
                TraversalSource::Flat { items, start } => TraversalSource::Flat {
                    items: Rc::clone(items),
                    start: *start,
                },
                TraversalSource::PrependHead { node } => TraversalSource::PrependHead {
                    node: Rc::clone(node),
                },
            },
            len: self.len,
            end: self.end,
        }
    }
}

impl<T> TraversalSegment<T> {
    fn get(&self, index: usize) -> Option<&T> {
        if index >= self.len {
            return None;
        }
        match &self.source {
            TraversalSource::Flat { items, start } => items.get(start + index),
            TraversalSource::PrependHead { node } => match node.as_ref() {
                AverListInner::Prepend { head, .. } if index == 0 => Some(head),
                _ => None,
            },
        }
    }
}

enum AverListInner<T> {
    Flat {
        items: Rc<Vec<T>>,
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
    Indexed {
        parts: Rc<Vec<TraversalSegment<T>>>,
        len: usize,
    },
}

fn take_list_inner<T>(list: &mut AverList<T>, pending: &mut Vec<Rc<AverListInner<T>>>) {
    pending.extend(list.inner.take());
}

fn detach_unique_children<T>(
    inner: &mut AverListInner<T>,
    pending: &mut Vec<Rc<AverListInner<T>>>,
) {
    match inner {
        AverListInner::Flat { .. } => {}
        AverListInner::Prepend { tail, .. } => take_list_inner(tail, pending),
        AverListInner::Concat { left, right, .. } => {
            take_list_inner(left, pending);
            take_list_inner(right, pending);
        }
        AverListInner::Segments { current, rest, .. } => {
            take_list_inner(current, pending);
            let rest_rc = std::mem::replace(rest, Rc::new(Vec::new()));
            if let Ok(mut rest_vec) = Rc::try_unwrap(rest_rc) {
                for part in &mut rest_vec {
                    take_list_inner(part, pending);
                }
            }
        }
        AverListInner::Indexed { parts, .. } => {
            let parts_rc = std::mem::replace(parts, Rc::new(Vec::new()));
            if let Ok(parts_vec) = Rc::try_unwrap(parts_rc) {
                for part in parts_vec {
                    if let TraversalSource::PrependHead { node } = part.source {
                        pending.push(node);
                    }
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

        let mut pending = Vec::new();

        // Detach unique children eagerly so deep list teardown does not recurse
        // through nested `Rc<AverListInner<_>>` chains on the Rust call stack.
        detach_unique_children(self, &mut pending);

        while let Some(child) = pending.pop() {
            if let Ok(mut child_inner) = Rc::try_unwrap(child) {
                detach_unique_children(&mut child_inner, &mut pending);
            }
        }
    }
}

#[derive(Clone)]
enum ListCursor<'a, T> {
    Node(&'a AverList<T>),
    Slice(&'a [T], usize),
    ListSlice(&'a [AverList<T>], usize),
    TraversalSlice(&'a [TraversalSegment<T>], usize),
    TraversalSegment(&'a TraversalSegment<T>, usize),
}

pub struct AverListIter<'a, T> {
    /// The top of the cursor stack, kept out of `stack` so a walk that never
    /// holds two cursors (an empty or a one-node list, a chain of prepends)
    /// does not allocate.
    top: Option<ListCursor<'a, T>>,
    stack: Vec<ListCursor<'a, T>>,
    remaining: usize,
}

impl<'a, T> AverListIter<'a, T> {
    #[inline]
    fn push(&mut self, cursor: ListCursor<'a, T>) {
        if let Some(previous) = self.top.replace(cursor) {
            self.stack.push(previous);
        }
    }

    #[inline]
    fn pop(&mut self) -> Option<ListCursor<'a, T>> {
        let cursor = self.top.take()?;
        self.top = self.stack.pop();
        Some(cursor)
    }
}

impl<T> Clone for AverList<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            start: self.start,
            segment_index: self.segment_index,
        }
    }
}

impl<T> AverList<T> {
    /// A list that is a whole node — every shape but a stepped-into flat body.
    fn node(inner: AverListInner<T>) -> Self {
        Self {
            inner: Some(Rc::new(inner)),
            start: 0,
            segment_index: 0,
        }
    }

    /// The node's shape, or none for the empty list.
    fn shape(&self) -> Option<&AverListInner<T>> {
        self.inner.as_deref()
    }

    #[cfg(test)]
    fn rc(&self) -> &Rc<AverListInner<T>> {
        self.inner.as_ref().expect("a non-empty list has a node")
    }

    fn concat_node(left: &Self, right: &Self) -> Self {
        Self::node(AverListInner::Concat {
            left: left.clone(),
            right: right.clone(),
            len: left.len() + right.len(),
        })
    }

    fn next_segment_end(parts: &[TraversalSegment<T>], len: usize) -> usize {
        parts.last().map_or(len, |part| part.end + len)
    }

    fn push_flat_segment(
        parts: &mut Vec<TraversalSegment<T>>,
        items: Rc<Vec<T>>,
        start: usize,
        len: usize,
    ) {
        if len == 0 {
            return;
        }
        let end = Self::next_segment_end(parts, len);
        parts.push(TraversalSegment {
            source: TraversalSource::Flat { items, start },
            len,
            end,
        });
    }

    fn push_prepend_head(parts: &mut Vec<TraversalSegment<T>>, node: Rc<AverListInner<T>>) {
        let end = Self::next_segment_end(parts, 1);
        parts.push(TraversalSegment {
            source: TraversalSource::PrependHead { node },
            len: 1,
            end,
        });
    }

    fn push_indexed_segment(
        parts: &mut Vec<TraversalSegment<T>>,
        segment: &TraversalSegment<T>,
        skip: usize,
    ) {
        if skip >= segment.len {
            return;
        }
        let len = segment.len - skip;
        let end = Self::next_segment_end(parts, len);
        let source = match &segment.source {
            TraversalSource::Flat { items, start } => TraversalSource::Flat {
                items: Rc::clone(items),
                start: start + skip,
            },
            TraversalSource::PrependHead { node } => {
                debug_assert_eq!(skip, 0);
                TraversalSource::PrependHead {
                    node: Rc::clone(node),
                }
            }
        };
        parts.push(TraversalSegment { source, len, end });
    }

    fn from_indexed(parts: Vec<TraversalSegment<T>>) -> Self {
        let Some(len) = parts.last().map(|part| part.end) else {
            return Self::empty();
        };
        Self::node(AverListInner::Indexed {
            parts: Rc::new(parts),
            len,
        })
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
        Self::node(AverListInner::Segments {
            current,
            rest,
            start,
            len,
        })
    }

    /// Compile a structural rope into a table of shared, ordered pieces.
    ///
    /// Values are never copied. `Concat` nodes are traversed once and each
    /// piece keeps an `Rc` to the storage it already had. Every table entry is
    /// constant-time: either a range in one flat body or one `Prepend` head.
    /// No entry may hide an arbitrary list, which would merely move the same
    /// shape-dependent traversal cost one level down.
    fn indexed(&self) -> Self {
        if matches!(self.shape(), Some(AverListInner::Indexed { .. })) {
            return self.clone();
        }

        let mut parts = Vec::new();
        let mut pending = vec![self];
        while let Some(list) = pending.pop() {
            let Some(node) = &list.inner else { continue };
            if list.is_empty() {
                continue;
            }
            match node.as_ref() {
                AverListInner::Flat { items } => {
                    Self::push_flat_segment(&mut parts, Rc::clone(items), list.start, list.len());
                }
                AverListInner::Prepend { tail, .. } => {
                    pending.push(tail);
                    Self::push_prepend_head(&mut parts, Rc::clone(node));
                }
                AverListInner::Concat { left, right, .. } => {
                    pending.push(right);
                    pending.push(left);
                }
                AverListInner::Segments {
                    current,
                    rest,
                    start,
                    ..
                } => {
                    for part in rest[*start..].iter().rev() {
                        pending.push(part);
                    }
                    pending.push(current);
                }
                AverListInner::Indexed {
                    parts: indexed_parts,
                    ..
                } => {
                    let previous_end = list
                        .segment_index
                        .checked_sub(1)
                        .map_or(0, |index| indexed_parts[index].end);
                    for (index, segment) in indexed_parts[list.segment_index..].iter().enumerate() {
                        let skip = if index == 0 {
                            list.start.saturating_sub(previous_end)
                        } else {
                            0
                        };
                        Self::push_indexed_segment(&mut parts, segment, skip);
                    }
                }
            }
        }
        Self::from_indexed(parts)
    }

    fn segment_for_offset(parts: &[TraversalSegment<T>], offset: usize) -> usize {
        parts.partition_point(|part| part.end <= offset)
    }

    fn indexed_tail(&self, len: usize, parts: &[TraversalSegment<T>]) -> Option<Self> {
        if self.start >= len {
            return None;
        }
        if self.start + 1 >= len {
            return Some(Self::empty());
        }

        let start = self.start + 1;
        let segment_index = if parts[self.segment_index].end <= start {
            self.segment_index + 1
        } else {
            self.segment_index
        };
        Some(Self {
            inner: self.inner.clone(),
            start,
            segment_index,
        })
    }

    fn indexed_head<'a>(&'a self, parts: &'a [TraversalSegment<T>]) -> Option<&'a T> {
        let previous_end = self
            .segment_index
            .checked_sub(1)
            .map_or(0, |index| parts[index].end);
        parts
            .get(self.segment_index)?
            .get(self.start.saturating_sub(previous_end))
    }

    fn indexed_drop(&self, n: usize, len: usize, parts: &[TraversalSegment<T>]) -> Self {
        if n == 0 {
            return self.clone();
        }
        if n == 1 {
            return self.indexed_tail(len, parts).unwrap_or_else(Self::empty);
        }
        let start = self.start.saturating_add(n);
        if start >= len {
            return Self::empty();
        }
        Self {
            inner: self.inner.clone(),
            start,
            segment_index: Self::segment_for_offset(parts, start),
        }
    }

    /// The rest of a flat body of `len` elements: the same node, read from one
    /// element further in.
    ///
    /// Nothing is built for the step — a walk costs the walk, not a node per
    /// element (issue #911). The last step hands back the empty list rather
    /// than a spent view, so a list walked to its end stops holding the body
    /// it walked over.
    fn flat_tail(&self, len: usize) -> Option<Self> {
        if self.start >= len {
            return None;
        }
        if self.start + 1 >= len {
            return Some(Self::empty());
        }
        Some(Self {
            inner: self.inner.clone(),
            start: self.start + 1,
            segment_index: 0,
        })
    }

    fn uncons(&self) -> Option<(&T, Self)> {
        match self.shape()? {
            AverListInner::Flat { items } => {
                let head = items.get(self.start)?;
                Some((head, self.flat_tail(items.len())?))
            }
            AverListInner::Prepend { head, tail, .. } => Some((head, tail.clone())),
            AverListInner::Concat { .. } => {
                let head = self.first()?;
                let indexed = self.indexed();
                let Some(AverListInner::Indexed { parts, len }) = indexed.shape() else {
                    unreachable!("a structural list produces a traversal index")
                };
                let tail = indexed.indexed_tail(*len, parts)?;
                Some((head, tail))
            }
            AverListInner::Segments {
                current,
                rest,
                start,
                ..
            } => {
                let (head, tail) = current.uncons()?;
                Some((head, Self::segments_rc(tail, Rc::clone(rest), *start)))
            }
            AverListInner::Indexed { parts, len } => {
                Some((self.indexed_head(parts)?, self.indexed_tail(*len, parts)?))
            }
        }
    }

    /// Skip the first `n` elements.
    ///
    /// A flat body is handed back as a view over the same allocation with the
    /// offset advanced — the slice `uncons` already hands out. Prepend and
    /// append-segment shapes cost what is stepped over. A concat rope first
    /// pays once to compile its nodes into a shared traversal index, then this
    /// and every later step only advance the constant-size cursor. Nothing is
    /// copied, so repeated stepping stays linear rather than quadratic (issues
    /// #913 and #1020).
    ///
    /// Sharing the body keeps the stepped-over elements alive for as long as
    /// the view is, which is the same trade `uncons` already makes.
    pub fn drop_first(&self, n: usize) -> Self {
        // The walk borrows; only the list the result starts at is cloned.
        let mut current = self;
        let mut remaining = n;

        loop {
            if remaining == 0 {
                return current.clone();
            }
            if remaining >= current.len() {
                return Self::empty();
            }

            let Some(shape) = current.shape() else {
                return Self::empty();
            };
            match shape {
                AverListInner::Flat { .. } => {
                    return Self {
                        inner: current.inner.clone(),
                        start: current.start + remaining,
                        segment_index: 0,
                    };
                }
                AverListInner::Prepend { tail, .. } => {
                    remaining -= 1;
                    current = tail;
                }
                AverListInner::Concat { .. } => {
                    return current.indexed().drop_first(remaining);
                }
                AverListInner::Segments {
                    current: head_segment,
                    rest,
                    start,
                    ..
                } => {
                    let mut segment = head_segment;
                    let mut index = *start;
                    while remaining >= segment.len() {
                        remaining -= segment.len();
                        match rest.get(index) {
                            Some(next) => {
                                segment = next;
                                index += 1;
                            }
                            None => return Self::empty(),
                        }
                    }
                    return Self::segments_rc(
                        segment.drop_first(remaining),
                        Rc::clone(rest),
                        index,
                    );
                }
                AverListInner::Indexed { parts, len } => {
                    return current.indexed_drop(remaining, *len, parts);
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
        Self {
            inner: None,
            start: 0,
            segment_index: 0,
        }
    }

    /// An empty vector with no room reserved is `[]`; one with room is kept,
    /// so a list builder fills it in place.
    pub fn from_vec(items: Vec<T>) -> Self {
        if items.capacity() == 0 {
            return Self::empty();
        }
        Self::node(AverListInner::Flat {
            items: Rc::new(items),
        })
    }

    /// O(1) if Flat with start=0, wraps existing Rc<Vec<T>> directly.
    pub fn from_rc_vec(items: Rc<Vec<T>>) -> Self {
        Self::node(AverListInner::Flat { items })
    }

    /// Extract the backing Rc<Vec<T>> — O(1) if Flat with start=0, O(n) otherwise.
    pub fn into_rc_vec(&self) -> Rc<Vec<T>>
    where
        T: Clone,
    {
        match self.shape() {
            Some(AverListInner::Flat { items }) if self.start == 0 => Rc::clone(items),
            _ => Rc::new(self.to_vec()),
        }
    }

    pub fn len(&self) -> usize {
        let Some(shape) = self.shape() else {
            return 0;
        };
        match shape {
            AverListInner::Flat { items } => items.len().saturating_sub(self.start),
            AverListInner::Prepend { len, .. }
            | AverListInner::Concat { len, .. }
            | AverListInner::Segments { len, .. } => *len,
            AverListInner::Indexed { len, .. } => len.saturating_sub(self.start),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        let mut current = self;
        let mut remaining = index;

        loop {
            match current.shape()? {
                AverListInner::Flat { items } => {
                    return items.get(current.start.saturating_add(remaining));
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
                AverListInner::Indexed { parts, len } => {
                    let offset = current.start.checked_add(remaining)?;
                    if offset >= *len {
                        return None;
                    }
                    let segment = Self::segment_for_offset(parts, offset);
                    let previous_end = segment.checked_sub(1).map_or(0, |index| parts[index].end);
                    return parts[segment].get(offset - previous_end);
                }
            }
        }
    }

    pub fn first(&self) -> Option<&T> {
        self.get(0)
    }

    pub fn as_slice(&self) -> Option<&[T]> {
        let Some(shape) = self.shape() else {
            return Some(&[]);
        };
        match shape {
            AverListInner::Flat { items } => Some(items.get(self.start..).unwrap_or(&[])),
            AverListInner::Prepend { .. }
            | AverListInner::Concat { .. }
            | AverListInner::Segments { .. }
            | AverListInner::Indexed { .. } => None,
        }
    }

    pub fn iter(&self) -> AverListIter<'_, T> {
        AverListIter {
            top: Some(ListCursor::Node(self)),
            stack: Vec::new(),
            remaining: self.len(),
        }
    }

    pub fn tail(&self) -> Option<Self> {
        match self.shape()? {
            AverListInner::Flat { items } => self.flat_tail(items.len()),
            AverListInner::Prepend { tail, .. } => Some(tail.clone()),
            AverListInner::Concat { .. }
            | AverListInner::Segments { .. }
            | AverListInner::Indexed { .. } => self.uncons().map(|(_, tail)| tail),
        }
    }

    /// One node, also onto the empty list: a one-element list is a
    /// `Prepend` onto `[]`, one allocation rather than a vector and its node.
    pub fn prepend(item: T, list: &Self) -> Self {
        let tail = if list.is_empty() {
            Self::empty()
        } else {
            list.clone()
        };
        Self::node(AverListInner::Prepend {
            head: item,
            len: tail.len() + 1,
            tail,
        })
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
        let singleton = Self::prepend(item, &Self::empty());
        if list.is_empty() {
            return singleton;
        }

        match list.shape() {
            Some(AverListInner::Segments {
                current,
                rest,
                start,
                ..
            }) => {
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
        while let Some(cursor) = self.pop() {
            match cursor {
                ListCursor::Slice(items, index) => {
                    if let Some(item) = items.get(index) {
                        self.push(ListCursor::Slice(items, index + 1));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(item);
                    }
                }
                ListCursor::Node(list) => match list.shape() {
                    None => {}
                    Some(AverListInner::Flat { items }) => {
                        let slice = items.get(list.start..).unwrap_or(&[]);
                        if !slice.is_empty() {
                            self.push(ListCursor::Slice(slice, 0));
                        }
                    }
                    Some(AverListInner::Prepend { head, tail, .. }) => {
                        self.push(ListCursor::Node(tail));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(head);
                    }
                    Some(AverListInner::Concat { left, right, .. }) => {
                        self.push(ListCursor::Node(right));
                        self.push(ListCursor::Node(left));
                    }
                    Some(AverListInner::Segments {
                        current,
                        rest,
                        start,
                        ..
                    }) => {
                        let slice = rest.get(*start..).unwrap_or(&[]);
                        if !slice.is_empty() {
                            self.push(ListCursor::ListSlice(slice, 0));
                        }
                        self.push(ListCursor::Node(current));
                    }
                    Some(AverListInner::Indexed { parts, .. }) => {
                        let previous_end = list
                            .segment_index
                            .checked_sub(1)
                            .map_or(0, |index| parts[index].end);
                        let offset = list.start.saturating_sub(previous_end);
                        if let Some(segment) = parts.get(list.segment_index) {
                            self.push(ListCursor::TraversalSlice(parts, list.segment_index + 1));
                            self.push(ListCursor::TraversalSegment(segment, offset));
                        }
                    }
                },
                ListCursor::ListSlice(items, index) => {
                    if let Some(item) = items.get(index) {
                        self.push(ListCursor::ListSlice(items, index + 1));
                        self.push(ListCursor::Node(item));
                    }
                }
                ListCursor::TraversalSlice(items, index) => {
                    if let Some(item) = items.get(index) {
                        self.push(ListCursor::TraversalSlice(items, index + 1));
                        self.push(ListCursor::TraversalSegment(item, 0));
                    }
                }
                ListCursor::TraversalSegment(segment, index) => {
                    if let Some(item) = segment.get(index) {
                        self.push(ListCursor::TraversalSegment(segment, index + 1));
                        self.remaining = self.remaining.saturating_sub(1);
                        return Some(item);
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
        let len = self.len();
        if len != other.len() {
            return false;
        }
        if len == 0 {
            return true;
        }
        // No shortcut for one node seen twice: a list of `Float` holding NaN
        // is not equal to itself.
        if let (Some(a), Some(b)) = (self.as_slice(), other.as_slice()) {
            return a == b;
        }
        self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}

impl<T: Eq> Eq for AverList<T> {}

/// Lexicographic, shorter-prefix-first — the order a map keyed on a list
/// iterates in. It has to match what the VM's key comparator and the proof
/// model state, because a claim about iteration order is checked against all
/// three.
impl<T: Ord> PartialOrd for AverList<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for AverList<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.iter().cmp(other.iter())
    }
}

impl<T: Hash> Hash for AverList<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        8u8.hash(state);
        self.len().hash(state);
        for item in self.iter() {
            item.hash(state);
        }
    }
}

/// Clamp an `Int` count for `List.take` / `List.drop` to a `usize`.
///
/// Negative counts step over nothing; counts past what a machine word can
/// address step over everything. Total by design — both builtins are defined
/// for every ℤ count, and every backend has to answer the same way.
pub fn clamp_list_count(n: &AverInt) -> usize {
    if *n <= AverInt::zero() {
        0
    } else {
        n.to_usize().unwrap_or(usize::MAX)
    }
}

pub fn list_uncons<T>(list: &AverList<T>) -> Option<(&T, AverList<T>)> {
    list.uncons()
}

pub trait AverListMatch: Sized {
    type Item;

    fn uncons_for_match(&self) -> Option<(Self::Item, Self)>;
}

impl<T: Clone> AverListMatch for AverList<T> {
    type Item = T;

    fn uncons_for_match(&self) -> Option<(Self::Item, Self)> {
        self.uncons_cloned()
    }
}

impl AverListMatch for AverIntList {
    type Item = AverInt;

    fn uncons_for_match(&self) -> Option<(Self::Item, Self)> {
        self.uncons_cloned()
    }
}

pub fn list_uncons_cloned<L: AverListMatch>(list: &L) -> Option<(L::Item, L)> {
    list.uncons_for_match()
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
        AverList, AverListInner, AverMap, AverStr, Buffer, LIST_APPEND_CHUNK_LIMIT, aver_display,
        env_set, string_slice,
    };

    #[test]
    fn buffer_separator_state_survives_an_empty_first_fragment() {
        let mut buffer = Buffer::with_capacity(0);
        if !buffer.is_empty() {
            buffer.push_str(",");
        }
        buffer.push_str("");
        if !buffer.is_empty() {
            buffer.push_str(",");
        }
        buffer.push_str("b");

        let rendered = AverStr::from(buffer);
        assert_eq!(rendered.as_ref(), ",b");
    }

    /// A literal that runs again and again is the one static node, never a
    /// new allocation; its clones are that node too and count nothing.
    #[test]
    fn a_string_literal_is_one_static_node() {
        let literal = || crate::str_lit!("Nat \"succ\"");
        let first = literal();
        for _ in 0..3 {
            let again = literal();
            assert!(super::Rc::ptr_eq(&first.0, &again.0));
            assert!(super::Rc::ptr_eq(&first.0, &again.clone().0));
        }
        assert!(super::Rc::is_static(&first.0));
        assert_eq!(first, AverStr::from("Nat \"succ\""));
        let threads: Vec<AverStr> = (0..4).map(|_| first.clone()).collect();
        std::thread::scope(|scope| {
            for mine in threads {
                scope.spawn(move || assert_eq!(&*mine, "Nat \"succ\""));
            }
        });
        assert_eq!(&*crate::str_lit!(""), "");
    }

    /// `AverMap` is exposed to generated programs, including programs that put
    /// network-derived values into a map. Keep the fast hasher keyed with a
    /// random per-process key instead of silently falling back to
    /// deterministic `FxHash` or Rust's slower default SipHash13. A new map,
    /// however it is built, copies that one key rather than drawing its own.
    #[test]
    fn aver_map_uses_keyed_ahash_state() {
        let map = AverMap::<u64, u64>::new().insert_owned(1, 2);
        let hasher = |map: &AverMap<u64, u64>| map.inner.as_ref().unwrap().hasher().clone();
        let _: ahash::RandomState = hasher(&map);
        let collected: AverMap<u64, u64> = [(1, 2)].into_iter().collect();
        let same_key = |other: &AverMap<u64, u64>| {
            hasher(&map).hash_one(42u64) == hasher(other).hash_one(42u64)
        };
        assert!(same_key(&AverMap::new().insert_owned(3, 4)));
        assert!(same_key(&collected));
    }

    /// `{}` has no table until its first write, which makes one of
    /// `MAP_FIRST_CAPACITY`; every operation reads the missing table as empty.
    #[test]
    fn an_empty_map_has_no_table_until_written() {
        let empty = AverMap::<u64, u64>::new();
        assert!(empty.inner.is_none());
        assert!(AverMap::<u64, u64>::default().inner.is_none());
        assert!(
            Vec::<(u64, u64)>::new()
                .into_iter()
                .collect::<AverMap<_, _>>()
                .inner
                .is_none()
        );
        assert_eq!(empty.get(&1), None);
        assert!(!empty.contains_key(&1));
        assert_eq!(empty.len(), 0);
        assert!(empty.is_empty());
        assert_eq!(empty.keys().count(), 0);
        assert_eq!(empty.values().count(), 0);
        assert_eq!(empty.iter().count(), 0);
        assert_eq!(empty.table_id(), 0);
        assert_eq!(format!("{empty:?}"), "{}");
        assert!(empty.remove(&1).inner.is_none());
        assert!(empty.clone().remove_owned(&1).inner.is_none());
        let mut rewritten = empty.clone();
        rewritten.rewrite_values_in_place(|v| *v += 1);
        assert!(!rewritten.rewrite_value_in_place(&1, |v| *v += 1));
        assert!(rewritten.inner.is_none());

        let one = empty.insert(1, 10);
        assert!(empty.inner.is_none());
        assert!(one.inner.as_ref().unwrap().capacity() >= super::MAP_FIRST_CAPACITY);
        assert_eq!(one[&1], 10);
        assert_eq!(one.len(), 1);

        // A table emptied by removal equals, and hashes as, a map with none.
        let emptied = one.remove(&1);
        assert!(emptied.inner.is_some());
        assert_eq!(emptied, empty);
        assert_eq!(empty, emptied);
        assert_ne!(one, empty);
        let hash = |m: &AverMap<u64, u64>| ahash::RandomState::with_seeds(1, 2, 3, 4).hash_one(m);
        assert_eq!(hash(&emptied), hash(&empty));
        assert_eq!(
            hash(&[(2, 3), (1, 2)].into_iter().collect()),
            hash(&empty.insert(1, 2).insert(2, 3))
        );
    }

    /// `==` on lists of every shape: empty, one node seen twice, flat bodies
    /// read as slices, mixed shapes and long ones.
    #[test]
    fn list_equality_over_shapes() {
        let flat = |v: &[i64]| AverList::from_vec(v.to_vec());
        let empty = AverList::<i64>::empty();
        assert_eq!(empty, AverList::from_vec(Vec::new()));
        assert_eq!(empty, flat(&[1]).tail().unwrap());
        assert_ne!(empty, flat(&[1]));

        let abc = flat(&[1, 2, 3]);
        assert_eq!(abc, abc.clone());
        assert_eq!(abc.tail().unwrap(), flat(&[2, 3]));
        assert_ne!(abc.tail().unwrap(), flat(&[2, 4]));
        assert_eq!(AverList::prepend(1, &flat(&[2, 3])), abc);
        assert_eq!(AverList::concat(&flat(&[1]), &flat(&[2, 3])), abc);
        assert_eq!(abc, AverList::append(&flat(&[1, 2]), 3));
        assert_ne!(AverList::concat(&flat(&[1]), &flat(&[2, 4])), abc);

        // A list of `Float` holding NaN is not equal to itself, even shared.
        let nan = AverList::from_vec(vec![f64::NAN]);
        assert_ne!(nan, nan.clone());
        assert_ne!(AverList::prepend(f64::NAN, &AverList::empty()), nan);

        let n = 10_000;
        let long = (0..n)
            .rev()
            .fold(AverList::empty(), |l, i| AverList::prepend(i, &l));
        let mut items: Vec<i64> = (0..n).collect();
        assert_eq!(long, AverList::from_vec(items.clone()));
        items[n as usize - 1] = -1;
        assert_ne!(long, AverList::from_vec(items));
        let halves = AverList::concat(
            &flat(&(0..n / 2).collect::<Vec<_>>()),
            &long.drop_first(n as usize / 2),
        );
        assert_eq!(halves, long);
        assert_eq!(
            halves.iter().copied().collect::<Vec<_>>(),
            (0..n).collect::<Vec<_>>()
        );
    }

    /// `[]` is no node at all, and a one-element list is one `Prepend` node
    /// onto it, where they used to be two and three allocations.
    #[test]
    fn an_empty_list_has_no_node_and_a_singleton_has_one() {
        let empty = AverList::<i32>::empty();
        assert!(empty.inner.is_none());
        assert!(AverList::<i32>::from_vec(Vec::new()).inner.is_none());

        let one = AverList::prepend(7, &empty);
        let AverListInner::Prepend { head, tail, len } = one.rc().as_ref() else {
            panic!("a singleton is a prepend node")
        };
        assert_eq!((*head, *len), (7, 1));
        assert!(tail.inner.is_none());
        assert_eq!(AverList::append(&empty, 7), one);
        assert_eq!(AverList::concat(&empty, &one), one);
        assert_eq!(
            super::list_uncons(&one).map(|(h, t)| (*h, t.len())),
            Some((7, 0))
        );

        assert!(super::list_uncons(&empty).is_none());
        assert!(empty.tail().is_none());
        assert!(empty.get(0).is_none());
        assert!(empty.iter().next().is_none());
        assert_eq!(empty.as_slice(), Some(&[][..]));
        assert!(empty.drop_first(3).is_empty());
        assert!(empty.into_rc_vec().is_empty());
    }

    /// One step off a flat list allocates nothing: the tail is the node it was
    /// taken from, read from one element further in. A fresh node per step is
    /// what every compiled list walk used to pay for (issue #911).
    #[test]
    fn unconsing_a_flat_list_steps_without_building_a_node() {
        let list = AverList::from_vec((0..64).collect::<Vec<i32>>());
        let (head, tail) = super::list_uncons(&list).expect("non-empty list unconses");

        assert_eq!(*head, 0);
        assert!(
            super::Rc::ptr_eq(list.rc(), tail.rc()),
            "uncons built a node for the tail instead of stepping the offset",
        );
        assert_eq!(tail.len(), 63);
        assert_eq!(tail.first(), Some(&1));
    }

    /// The same for `tail`, which reaches the flat step by its own path.
    #[test]
    fn tail_of_a_flat_list_steps_without_building_a_node() {
        let list = AverList::from_vec((0..8).collect::<Vec<i32>>());
        let tail = list.tail().expect("non-empty list has a tail");

        assert!(
            super::Rc::ptr_eq(list.rc(), tail.rc()),
            "tail built a node instead of stepping the offset",
        );
        assert_eq!(tail.to_vec(), (1..8).collect::<Vec<i32>>());
    }

    /// Per element, not just for the first one: walking a flat list end to end
    /// stays on the one node the list started as. The last step is the empty
    /// list, which stops holding the body it walked over.
    #[test]
    fn walking_a_flat_list_stays_on_one_node() {
        let list = AverList::from_vec((0..256).collect::<Vec<i32>>());
        let mut rest = list.clone();
        let mut seen = Vec::new();

        while let Some((head, tail)) = super::list_uncons(&rest) {
            seen.push(*head);
            if !tail.is_empty() {
                assert!(
                    super::Rc::ptr_eq(list.rc(), tail.rc()),
                    "the walk left the node it started on at element {}",
                    seen.len(),
                );
            }
            rest = tail;
        }

        assert_eq!(seen, (0..256).collect::<Vec<i32>>());
    }

    #[test]
    fn prepend_and_tail_share_structure() {
        let base = AverList::from_vec(vec![2, 3]);
        let full = AverList::prepend(1, &base);
        assert_eq!(full.first(), Some(&1));
        assert_eq!(full.tail().unwrap(), base);
    }

    /// `drop_first` is `prepend_and_tail_share_structure` one step further
    /// along: the list handed back is a view over the body it was given, at an
    /// advanced offset, so stepping over a prefix copies nothing (issue #913).
    #[test]
    fn drop_first_shares_the_flat_body_and_advances_the_offset() {
        let list = AverList::from_vec((0..64).collect::<Vec<i32>>());
        let stepped = list.drop_first(16);

        let source = list.as_slice().expect("a flat list has a slice");
        let view = stepped.as_slice().expect("a flat view has a slice");
        assert!(
            std::ptr::eq(&source[16], &view[0]),
            "drop_first copied the remainder instead of viewing the body",
        );
        assert_eq!(view.len(), 48);
        assert_eq!(view[0], 16);
    }

    #[test]
    fn drop_first_of_nothing_returns_the_same_list() {
        let list = AverList::from_vec(vec![1, 2, 3]);
        let stepped = list.drop_first(0);

        let source = list.as_slice().expect("a flat list has a slice");
        let view = stepped.as_slice().expect("a flat list has a slice");
        assert!(std::ptr::eq(&source[0], &view[0]));
        assert_eq!(stepped.len(), 3);
    }

    /// Past the end is the empty list, and it is the same empty list that
    /// unconsing to the end arrives at — the two walks must stay
    /// interchangeable at their last step.
    #[test]
    fn drop_first_past_the_end_matches_unconsing_to_the_end() {
        let list = AverList::from_vec(vec![1, 2, 3]);
        let mut rest = list.clone();
        while let Some((_, tail)) = super::list_uncons(&rest) {
            rest = tail;
        }

        for count in [3, 4, 4_000] {
            let stepped = list.drop_first(count);
            assert!(stepped.is_empty(), "drop_first({count}) is not empty");
            assert_eq!(stepped, rest, "drop_first({count}) != unconsed to the end");
        }
    }

    #[test]
    fn drop_first_walks_a_prepend_chain_onto_the_shared_body() {
        let base = AverList::from_vec(vec![2, 3, 4, 5]);
        let list = AverList::prepend(0, &AverList::prepend(1, &base));

        let stepped = list.drop_first(2);

        let source = base.as_slice().expect("a flat list has a slice");
        let view = stepped.as_slice().expect("the walk lands on a flat body");
        assert!(
            std::ptr::eq(&source[0], &view[0]),
            "stepping over a prepend chain rebuilt the body it arrived at",
        );
        assert_eq!(stepped.to_vec(), vec![2, 3, 4, 5]);
    }

    /// Stepping into the middle of a prepend chain hands back the node it
    /// lands on, and holds nothing else: the nodes stepped over keep the
    /// counts they had.
    #[test]
    fn drop_first_into_a_prepend_chain_holds_only_the_node_it_lands_on() {
        let base = AverList::from_vec(vec![3, 4]);
        let third = AverList::prepend(2, &base);
        let second = AverList::prepend(1, &third);
        let list = AverList::prepend(0, &second);
        let counts = |l: &AverList<i32>| super::Rc::strong_count(l.rc());
        let before = [counts(&list), counts(&second), counts(&third)];

        let stepped = list.drop_first(2);

        assert!(super::Rc::ptr_eq(stepped.rc(), third.rc()));
        assert_eq!(
            [counts(&list), counts(&second), counts(&third)],
            [before[0], before[1], before[2] + 1],
        );
        assert_eq!(stepped.to_vec(), vec![2, 3, 4]);
    }

    /// Stepping into the left half of a concat must keep the right half. The
    /// trap `uncons` carries a comment about: a node reached down the left
    /// spine has right-siblings waiting, and forgetting them silently deletes
    /// everything after the step.
    #[test]
    fn drop_first_into_a_segmented_left_half_keeps_the_right_half() {
        let mut appended = AverList::empty();
        for value in 0..200 {
            appended = AverList::append(&appended, value);
        }
        let joined = AverList::concat(&appended, &AverList::from_vec(vec![999]));

        let stepped = joined.drop_first(150);

        assert_eq!(stepped.len(), 51, "the step lost the right half");
        assert_eq!(stepped.first(), Some(&150));
        assert_eq!(stepped.to_vec().last().copied(), Some(999));
    }

    /// The same trap in `uncons` itself: destructuring a concat whose left
    /// half is a segmented append chain used to drop the right half whole.
    #[test]
    fn uncons_of_a_concat_over_a_segmented_left_keeps_the_right_half() {
        let mut appended = AverList::empty();
        for value in 0..200 {
            appended = AverList::append(&appended, value);
        }
        let joined = AverList::concat(&appended, &AverList::from_vec(vec![999]));

        let (head, tail) = super::list_uncons(&joined).expect("non-empty list unconses");

        assert_eq!(*head, 0);
        assert_eq!(
            tail.len(),
            200,
            "uncons dropped the right half of the concat"
        );
        assert_eq!(tail.to_vec().last().copied(), Some(999));
    }

    /// The reporter's walk, on every shape a list can be built into: stepping
    /// with `drop_first` must see exactly what stepping by destructuring sees.
    #[test]
    fn walking_by_drop_first_agrees_with_walking_by_uncons() {
        let flat = AverList::from_vec((0..40).collect::<Vec<i32>>());
        let mut prepended = flat.clone();
        for value in (100..110).rev() {
            prepended = AverList::prepend(value, &prepended);
        }
        let mut appended = AverList::empty();
        for value in 0..300 {
            appended = AverList::append(&appended, value);
        }
        let joined = AverList::concat(&prepended, &appended);

        for list in [flat, prepended, appended, joined] {
            for step in [1, 3, 7, 64] {
                let mut by_drop = list.clone();
                let mut by_uncons = list.clone();
                loop {
                    assert_eq!(
                        by_drop.to_vec(),
                        by_uncons.to_vec(),
                        "a walk in steps of {step} diverged from destructuring",
                    );
                    if by_uncons.is_empty() {
                        break;
                    }
                    by_drop = by_drop.drop_first(step);
                    for _ in 0..step {
                        by_uncons = match super::list_uncons(&by_uncons) {
                            Some((_, tail)) => tail,
                            None => break,
                        };
                    }
                }
            }
        }
    }

    #[test]
    fn vector_materialization_boundary_is_inclusive_and_owns_its_message() {
        let limit = super::MAX_MATERIALIZED_VECTOR_ELEMENTS;
        let at_limit = super::AverInt::from_i64(limit as i64);
        let above_limit = super::AverInt::from_i64(limit as i64 + 1);
        let negative = super::AverInt::from_i64(-1);

        assert_eq!(super::checked_vector_size(&at_limit), Some(limit));
        assert_eq!(super::checked_vector_size(&above_limit), None);
        assert_eq!(super::checked_vector_size(&negative), None);
        assert_eq!(
            super::vector_size_error_message(),
            format!("Vector.new: size must be between 0 and {limit}")
        );
    }

    /// Sharing must stay invisible to programs. A `Vector` hands its backing
    /// allocation to `List.fromVector` without copying, and `drop_first` hands
    /// on the very same allocation — so a write to the vector has to
    /// copy-on-write, or the view would see a value that was never in the list
    /// it was taken from.
    #[test]
    fn writing_to_a_vector_does_not_reach_a_view_sharing_its_allocation() {
        let vector = super::AverVector::from_vec(vec![1, 2, 3, 4]);
        let list = vector.to_list();
        let view = list.drop_first(2);

        let written = vector
            .clone()
            .set_owned(3, 99)
            .expect("index 3 is in bounds");

        assert_eq!(view.to_vec(), vec![3, 4], "a write reached a shared view");
        assert_eq!(list.to_vec(), vec![1, 2, 3, 4]);
        assert_eq!(written.get(3), Some(&99));
        assert_eq!(vector.get(3), Some(&4));
    }

    #[test]
    fn concat_and_iter_preserve_order() {
        let left = AverList::from_vec(vec![1, 2]);
        let right = AverList::from_vec(vec![3, 4]);
        let joined = AverList::concat(&left, &right);
        assert_eq!(joined.to_vec(), vec![1, 2, 3, 4]);
    }

    /// The first destructuring step compiles either concat topology into one
    /// traversal index. Every later tail advances the constant-size cursor on
    /// that same table instead of rebuilding any part of the rope (#1020).
    #[test]
    fn concat_walks_share_one_traversal_index() {
        let part = AverList::from_vec(vec![1, 2, 3, 4]);
        let mut right = AverList::empty();
        let mut left = AverList::empty();
        for _ in 0..32 {
            right = AverList::concat(&part, &right);
            left = AverList::concat(&left, &part);
        }

        for list in [right, left] {
            let (_, first_tail) = super::list_uncons(&list).expect("concat unconses");
            let table = match first_tail.rc().as_ref() {
                AverListInner::Indexed { parts, .. } => super::Rc::clone(parts),
                other => panic!("concat tail was not indexed: {}", aver_display_shape(other)),
            };

            let mut rest = first_tail;
            while let Some((_, tail)) = super::list_uncons(&rest) {
                if !tail.is_empty() {
                    let AverListInner::Indexed { parts, .. } = tail.rc().as_ref() else {
                        panic!("indexed traversal changed representation")
                    };
                    assert!(
                        super::Rc::ptr_eq(&table, parts),
                        "a tail rebuilt the traversal table",
                    );
                }
                rest = tail;
            }
        }
    }

    #[test]
    fn drop_first_advances_the_same_concat_traversal_index() {
        let mut list = AverList::empty();
        for value in 0..64 {
            list = AverList::concat(&list, &AverList::from_vec(vec![value]));
        }
        let (_, tail) = super::list_uncons(&list).expect("concat unconses");
        let table = match tail.rc().as_ref() {
            AverListInner::Indexed { parts, .. } => super::Rc::clone(parts),
            other => panic!("expected Indexed, got {}", aver_display_shape(other)),
        };

        let dropped = tail.drop_first(30);
        let AverListInner::Indexed { parts, .. } = dropped.rc().as_ref() else {
            panic!("drop_first changed the indexed representation")
        };
        assert!(super::Rc::ptr_eq(&table, parts));
        assert_eq!(dropped.first(), Some(&31));
        assert_eq!(dropped.len(), 33);
    }

    /// A traversal view can participate in another O(1) concat. Compiling the
    /// new rope copies its atomic descriptors, not the values and not one
    /// descriptor per remaining element.
    #[test]
    fn concat_over_an_indexed_tail_reuses_its_segments() {
        let part = AverList::from_vec(vec![1, 2, 3, 4]);
        let mut list = AverList::empty();
        for _ in 0..16 {
            list = AverList::concat(&list, &part);
        }
        let (_, indexed_tail) = super::list_uncons(&list).expect("concat unconses");
        let old_segment_count = match indexed_tail.rc().as_ref() {
            AverListInner::Indexed { parts, .. } => parts.len(),
            other => panic!("expected Indexed, got {}", aver_display_shape(other)),
        };

        let suffix = AverList::from_vec(vec![9, 10]);
        let joined = AverList::concat(&indexed_tail, &suffix);
        let (_, joined_tail) = super::list_uncons(&joined).expect("joined view unconses");
        let new_segment_count = match joined_tail.rc().as_ref() {
            AverListInner::Indexed { parts, .. } => parts.len(),
            other => panic!("expected Indexed, got {}", aver_display_shape(other)),
        };

        assert!(new_segment_count <= old_segment_count + 1);
        let expected = indexed_tail
            .iter()
            .copied()
            .chain([9, 10])
            .skip(1)
            .collect::<Vec<_>>();
        assert_eq!(joined_tail.to_vec(), expected);
    }

    /// Index construction is an explicit-stack DFS. Both adversarial rope
    /// spines must survive a depth that would overflow a recursive walk.
    #[test]
    fn indexing_deep_concat_spines_does_not_use_the_call_stack() {
        let singleton = AverList::from_vec(vec![7]);
        let mut left = AverList::empty();
        let mut right = AverList::empty();
        for _ in 0..20_000 {
            left = AverList::concat(&left, &singleton);
            right = AverList::concat(&singleton, &right);
        }

        for list in [left, right] {
            let (head, tail) = super::list_uncons(&list).expect("deep concat unconses");
            assert_eq!(*head, 7);
            assert_eq!(tail.len(), 19_999);
            assert!(matches!(tail.rc().as_ref(), AverListInner::Indexed { .. }));
        }
    }

    /// A `PrependHead` descriptor owns its prepend node, and that node owns its
    /// tail. The traversal table therefore deliberately retains the original
    /// prepend topology (including the consumed prefix) until the indexed view
    /// dies. Those overlapping suffix references must form no cycle or leak,
    /// and releasing them must use the iterative teardown path.
    #[test]
    fn indexed_prepend_retention_releases_every_value_iteratively() {
        struct Counted(super::Rc<std::sync::atomic::AtomicUsize>);

        impl Drop for Counted {
            fn drop(&mut self) {
                self.0.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
        }

        const PREPENDED: usize = 50_000;
        let drops = super::Rc::new(std::sync::atomic::AtomicUsize::new(0));
        let mut prepended = AverList::empty();
        for _ in 0..PREPENDED {
            prepended = AverList::prepend(Counted(super::Rc::clone(&drops)), &prepended);
        }
        let suffix = AverList::from_vec(vec![Counted(super::Rc::clone(&drops))]);
        let joined = AverList::concat(&prepended, &suffix);
        let indexed_tail = {
            let (_, tail) = super::list_uncons(&joined).expect("joined prepend unconses");
            tail
        };

        drop(joined);
        drop(prepended);
        drop(suffix);
        assert_eq!(
            drops.load(std::sync::atomic::Ordering::Relaxed),
            0,
            "the indexed view should retain every shared source node",
        );

        drop(indexed_tail);
        assert_eq!(
            drops.load(std::sync::atomic::Ordering::Relaxed),
            PREPENDED + 1,
            "dropping the indexed view leaked a retained value",
        );
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

        match list.rc().as_ref() {
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
            AverListInner::Indexed { .. } => "Indexed",
        }
    }
}
