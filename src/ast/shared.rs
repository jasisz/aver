//! A shared, copy-on-write node pointer for the resolved tree.
//!
//! Cloning a `Shared<T>` bumps a reference count instead of copying the
//! subtree under it; writing through it (`DerefMut`) copies the node only
//! when another owner still holds it. `Debug` and `PartialEq` read through
//! to the value, so the tree prints and compares exactly as with `Box`.

use std::sync::Arc;

pub struct Shared<T>(Arc<T>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Shared(Arc::new(value))
    }

    /// Whether both point at the same node.
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        Arc::ptr_eq(&a.0, &b.0)
    }
}

impl<T: Clone> Shared<T> {
    /// The value, copied only when another owner still holds it.
    pub fn unwrap(this: Self) -> T {
        Arc::unwrap_or_clone(this.0)
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Arc::clone(&self.0))
    }
}

impl<T> std::ops::Deref for Shared<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: Clone> std::ops::DerefMut for Shared<T> {
    fn deref_mut(&mut self) -> &mut T {
        Arc::make_mut(&mut self.0)
    }
}

impl<T> AsRef<T> for Shared<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> std::borrow::Borrow<T> for Shared<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Shared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (*self.0).fmt(f)
    }
}

impl<T: PartialEq> PartialEq for Shared<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl<T: PartialEq> PartialEq<T> for Shared<T> {
    fn eq(&self, other: &T) -> bool {
        *self.0 == *other
    }
}

impl<T> From<T> for Shared<T> {
    fn from(value: T) -> Self {
        Shared::new(value)
    }
}

impl<T> From<Box<T>> for Shared<T> {
    fn from(value: Box<T>) -> Self {
        Shared(Arc::from(value))
    }
}

impl<T> Default for Shared<Vec<T>> {
    fn default() -> Self {
        Shared::new(Vec::new())
    }
}

impl<T> FromIterator<T> for Shared<Vec<T>> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Shared::new(iter.into_iter().collect())
    }
}

impl<'a, T> IntoIterator for &'a Shared<Vec<T>> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, T: Clone> IntoIterator for &'a mut Shared<Vec<T>> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        Arc::make_mut(&mut self.0).iter_mut()
    }
}

impl<T: Clone> IntoIterator for Shared<Vec<T>> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        Arc::unwrap_or_clone(self.0).into_iter()
    }
}

/// A list of nodes, shared.
pub fn vec<T>(items: Vec<T>) -> Shared<Vec<T>> {
    Shared::new(items)
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Spanned, Stmt, TopLevel, Type};

    /// The first argument of the call the function body ends with.
    fn first_arg(item: &TopLevel) -> &Spanned<Expr> {
        let TopLevel::FnDef(fd) = item else {
            panic!("a function")
        };
        let Some(Stmt::Expr(body)) = fd.body.stmts().last() else {
            panic!("a body")
        };
        let Expr::FnCall(_, args) = &body.node else {
            panic!("a call")
        };
        &args[0]
    }

    #[test]
    fn a_clone_shares_stamps_and_an_unshared_copy_does_not() {
        let items =
            crate::source::parse_source("fn f(x: Int) -> Int\n    g(Int.abs(x))\n").unwrap();
        let item = items
            .iter()
            .find(|i| matches!(i, TopLevel::FnDef(_)))
            .unwrap();

        let unshared = item.unshared();
        first_arg(&unshared).set_ty(Type::Str);
        assert!(first_arg(item).ty().is_none());

        let clone = item.clone();
        first_arg(&clone).set_ty(Type::Int);
        assert_eq!(first_arg(item).ty(), Some(&Type::Int));

        // A stamp already set is kept by the copy, in a cell of its own.
        let again = item.unshared();
        assert_eq!(first_arg(&again).ty(), Some(&Type::Int));
        assert!(!std::ptr::eq(first_arg(&again), first_arg(item)));
    }
}
