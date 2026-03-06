use crate::AverList;

pub fn aver_display<T: AverDisplay>(val: &T) -> String {
    val.aver_display()
}

pub trait AverDisplay {
    fn aver_display(&self) -> String;

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl AverDisplay for i64 {
    fn aver_display(&self) -> String {
        self.to_string()
    }
}

impl AverDisplay for f64 {
    fn aver_display(&self) -> String {
        self.to_string()
    }
}

impl AverDisplay for String {
    fn aver_display(&self) -> String {
        self.clone()
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

impl<A: AverDisplay, B: AverDisplay> AverDisplay for (A, B) {
    fn aver_display(&self) -> String {
        format!(
            "({}, {})",
            self.0.aver_display_inner(),
            self.1.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl<A: AverDisplay, B: AverDisplay, C: AverDisplay> AverDisplay for (A, B, C) {
    fn aver_display(&self) -> String {
        format!(
            "({}, {}, {})",
            self.0.aver_display_inner(),
            self.1.aver_display_inner(),
            self.2.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}
