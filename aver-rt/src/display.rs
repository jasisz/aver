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

/// Appends the decimal digits of `n`, what `{n}` writes, without going
/// through a `fmt::Formatter`.
fn push_i64(out: &mut String, n: i64) {
    let mut digits = [0u8; 20];
    let mut at = digits.len();
    let mut rest = n.unsigned_abs();
    loop {
        at -= 1;
        digits[at] = b'0' + (rest % 10) as u8;
        rest /= 10;
        if rest == 0 {
            break;
        }
    }
    if n < 0 {
        out.push('-');
    }
    // SAFETY: only ASCII digits were written to `digits[at..]`.
    out.push_str(unsafe { std::str::from_utf8_unchecked(&digits[at..]) });
}

impl AverDisplay for i64 {
    fn aver_display(&self) -> String {
        self.to_string()
    }

    fn push_display(&self, out: &mut String) {
        push_i64(out, *self);
    }
}

impl AverDisplay for crate::AverInt {
    fn aver_display(&self) -> String {
        // `AverInt`'s `Display` renders the bare decimal (Small or Big),
        // byte-identical to the VM's `Value::Int` display.
        self.to_string()
    }

    fn push_display(&self, out: &mut String) {
        match self.to_i64() {
            Some(n) => push_i64(out, n),
            None => {
                let _ = write!(out, "{self}");
            }
        }
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

#[cfg(test)]
mod tests {
    use super::AverDisplay;
    use crate::AverInt;

    /// An `Int` written into a buffer reads as its `Display`, at the ends of
    /// `i64`, around the inline range and past `i64` either way.
    #[test]
    fn an_int_is_written_as_its_display() {
        let mut texts = vec![
            "0".to_string(),
            "-0".to_string(),
            "9".to_string(),
            "-10".to_string(),
            "4611686018427387904".to_string(),
            "-4611686018427387905".to_string(),
            "99999999999999999999999999".to_string(),
            "-99999999999999999999999999".to_string(),
        ];
        for n in [i64::MIN, i64::MIN + 1, -1, 1, i64::MAX - 1, i64::MAX] {
            texts.push(n.to_string());
        }
        for text in texts {
            let n: AverInt = text.parse().expect("an integer");
            let mut out = String::from("<");
            n.push_display(&mut out);
            assert_eq!(out, format!("<{n}"), "{text}");
            if let Some(word) = n.to_i64() {
                let mut out = String::from("<");
                word.push_display(&mut out);
                assert_eq!(out, format!("<{word}"), "{text}");
            }
        }
    }
}
