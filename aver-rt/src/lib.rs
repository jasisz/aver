#![doc = include_str!("../README.md")]

mod display;
#[cfg(feature = "http")]
pub mod http;
pub mod http_server;
mod runtime;
mod service_types;
pub mod tcp;

pub use display::{AverDisplay, aver_display};
pub use runtime::{
    append_text, console_error, console_print, console_warn, delete_dir, delete_file, env_get,
    env_set, list_dir, make_dir, path_exists, read_line, read_text, string_slice, time_now,
    time_sleep, time_unix_ms, write_text,
};
pub use service_types::{Header, HttpRequest, HttpResponse, TcpConnection};

use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FusedIterator;
use std::rc::Rc;

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
}

#[derive(Clone)]
enum ListCursor<'a, T> {
    Node(&'a AverList<T>),
    Slice(&'a [T], usize),
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

    pub fn len(&self) -> usize {
        match self.inner.as_ref() {
            AverListInner::Flat { items, start } => items.len().saturating_sub(*start),
            AverListInner::Prepend { len, .. } | AverListInner::Concat { len, .. } => *len,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.iter().nth(index)
    }

    pub fn first(&self) -> Option<&T> {
        self.iter().next()
    }

    pub fn as_slice(&self) -> Option<&[T]> {
        match self.inner.as_ref() {
            AverListInner::Flat { items, start } => Some(items.get(*start..).unwrap_or(&[])),
            AverListInner::Prepend { .. } | AverListInner::Concat { .. } => None,
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
            AverListInner::Flat { items, start } => {
                if *start >= items.len() {
                    None
                } else {
                    Some(Self {
                        inner: Rc::new(AverListInner::Flat {
                            items: Rc::clone(items),
                            start: start + 1,
                        }),
                    })
                }
            }
            AverListInner::Prepend { tail, .. } => Some(tail.clone()),
            AverListInner::Concat { left, right, .. } => {
                let left_len = left.len();
                if left_len == 0 {
                    right.tail()
                } else if left_len == 1 {
                    Some(right.clone())
                } else {
                    let left_tail = left.tail().expect("non-empty left side must have a tail");
                    Some(Self::concat(&left_tail, right))
                }
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
        Self {
            inner: Rc::new(AverListInner::Concat {
                left: left.clone(),
                right: right.clone(),
                len: left.len() + right.len(),
            }),
        }
    }

    pub fn append(list: &Self, item: T) -> Self {
        Self::concat(list, &Self::from_vec(vec![item]))
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
                },
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
    let head = list.first()?;
    let tail = list.tail().expect("non-empty list must have a tail");
    Some((head, tail))
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
    use super::{AverList, aver_display, env_set, string_slice};

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
    fn aver_display_quotes_strings_inside_lists() {
        let parts = AverList::from_vec(vec!["a".to_string(), "b".to_string()]);
        assert_eq!(aver_display(&parts), "[\"a\", \"b\"]");
    }

    #[test]
    fn string_slice_uses_code_point_indices() {
        assert_eq!(string_slice("zażółć", 1, 4), "ażó");
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
}
