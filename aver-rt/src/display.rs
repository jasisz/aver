use crate::AverList;
use std::fmt::Write;

pub fn aver_display<T: AverDisplay>(val: &T) -> String {
    val.aver_display()
}

pub trait AverDisplay {
    fn aver_display(&self) -> String;

    /// Appends what [`AverDisplay::aver_display`] returns to `out`; the
    /// scalars and strings write it there without a `String` of their own.
    fn push_display(&self, out: &mut String) {
        out.push_str(&self.aver_display());
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl AverDisplay for i64 {
    fn aver_display(&self) -> String {
        self.to_string()
    }

    fn push_display(&self, out: &mut String) {
        let _ = write!(out, "{self}");
    }
}

impl AverDisplay for crate::AverInt {
    fn aver_display(&self) -> String {
        // `AverInt`'s `Display` renders the bare decimal (Small or Big),
        // byte-identical to the VM's `Value::Int` display.
        self.to_string()
    }

    fn push_display(&self, out: &mut String) {
        let _ = write!(out, "{self}");
    }
}

impl AverDisplay for f64 {
    fn aver_display(&self) -> String {
        self.to_string()
    }

    fn push_display(&self, out: &mut String) {
        let _ = write!(out, "{self}");
    }
}

impl AverDisplay for String {
    fn aver_display(&self) -> String {
        self.clone()
    }

    fn push_display(&self, out: &mut String) {
        out.push_str(self);
    }

    fn aver_display_inner(&self) -> String {
        format!("\"{}\"", self)
    }
}

impl AverDisplay for crate::AverStr {
    fn aver_display(&self) -> String {
        self.to_string()
    }

    fn push_display(&self, out: &mut String) {
        out.push_str(self);
    }

    fn aver_display_inner(&self) -> String {
        format!("\"{}\"", self)
    }
}

impl AverDisplay for bool {
    fn aver_display(&self) -> String {
        if *self {
            "true".to_string()
        } else {
            "false".to_string()
        }
    }

    fn push_display(&self, out: &mut String) {
        out.push_str(if *self { "true" } else { "false" });
    }
}

impl AverDisplay for () {
    fn aver_display(&self) -> String {
        "()".to_string()
    }
}

impl<T: AverDisplay, E: AverDisplay> AverDisplay for Result<T, E> {
    fn aver_display(&self) -> String {
        match self {
            Ok(v) => format!("Result.Ok({})", v.aver_display_inner()),
            Err(e) => format!("Result.Err({})", e.aver_display_inner()),
        }
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<T: AverDisplay> AverDisplay for Option<T> {
    fn aver_display(&self) -> String {
        match self {
            Some(v) => format!("Option.Some({})", v.aver_display_inner()),
            None => "Option.None".to_string(),
        }
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<T: AverDisplay> AverDisplay for Box<T> {
    fn aver_display(&self) -> String {
        (**self).aver_display()
    }

    fn aver_display_inner(&self) -> String {
        (**self).aver_display_inner()
    }
}

impl<T: AverDisplay> AverDisplay for Vec<T> {
    fn aver_display(&self) -> String {
        let parts: Vec<String> = self.iter().map(|x| x.aver_display_inner()).collect();
        format!("[{}]", parts.join(", "))
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<T: AverDisplay> AverDisplay for AverList<T> {
    fn aver_display(&self) -> String {
        let parts: Vec<String> = self.iter().map(|x| x.aver_display_inner()).collect();
        format!("[{}]", parts.join(", "))
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<K: AverDisplay + Eq + std::hash::Hash + Ord, V: AverDisplay> AverDisplay
    for std::collections::HashMap<K, V>
{
    fn aver_display(&self) -> String {
        let mut keys: Vec<&K> = self.keys().collect();
        keys.sort();
        let parts: Vec<String> = keys
            .iter()
            .map(|k| {
                format!(
                    "{}: {}",
                    k.aver_display_inner(),
                    self[*k].aver_display_inner()
                )
            })
            .collect();
        format!("{{{}}}", parts.join(", "))
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<K: AverDisplay + Clone + Eq + std::hash::Hash + Ord, V: AverDisplay + Clone> AverDisplay
    for crate::AverMap<K, V>
{
    fn aver_display(&self) -> String {
        let mut keys: Vec<&K> = self.keys().collect();
        keys.sort();
        let parts: Vec<String> = keys
            .iter()
            .map(|k| {
                format!(
                    "{}: {}",
                    k.aver_display_inner(),
                    self[k].aver_display_inner()
                )
            })
            .collect();
        format!("{{{}}}", parts.join(", "))
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<T: AverDisplay + Clone> AverDisplay for crate::AverVector<T> {
    fn aver_display(&self) -> String {
        let parts: Vec<String> = self.iter().map(|v| v.aver_display_inner()).collect();
        format!("Vector[{}]", parts.join(", "))
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// Tuples display as `(a, b, ...)`, every item in its inner form, as the VM
/// prints them. Rust implements `Eq`, `Ord` and `Hash` for tuples of up to
/// twelve items, so that is also the largest tuple a `Map` key can be.
macro_rules! tuple_display {
    ($($name:ident : $idx:tt),+) => {
        impl<$($name: AverDisplay),+> AverDisplay for ($($name,)+) {
            fn aver_display(&self) -> String {
                let parts = [$(self.$idx.aver_display_inner()),+];
                format!("({})", parts.join(", "))
            }

            fn aver_display_inner(&self) -> String {
                self.aver_display()
            }
        }
    };
}

tuple_display!(A: 0, B: 1);
tuple_display!(A: 0, B: 1, C: 2);
tuple_display!(A: 0, B: 1, C: 2, D: 3);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10);
tuple_display!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11);
