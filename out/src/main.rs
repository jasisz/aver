#![allow(unused_variables, unused_mut, dead_code, unused_imports, unused_parens, non_snake_case, non_camel_case_types, unreachable_patterns)]
use std::collections::HashMap;

mod aver_rt {
    use std::fmt;

    /// Read a line from stdin, matching Aver's Console.readLine semantics.
    pub fn read_line() -> Result<String, String> {
        let mut buf = String::new();
        std::io::stdin()
            .read_line(&mut buf)
            .map_err(|e| e.to_string())?;
        if buf.ends_with('\n') {
            buf.pop();
            if buf.ends_with('\r') {
                buf.pop();
            }
        }
        Ok(buf)
    }

    /// Code-point based string slice, matching Aver's String.slice semantics.
    pub fn string_slice(s: &str, from: i64, to: i64) -> String {
        let chars: Vec<char> = s.chars().collect();
        let len = chars.len() as i64;
        let start = from.max(0) as usize;
        let end = to.min(len) as usize;
        if start >= end || start >= chars.len() {
            return String::new();
        }
        chars[start..end].iter().collect()
    }

    /// List directory contents, matching Aver's Disk.listDir semantics.
    pub fn list_dir(path: &str) -> Result<Vec<String>, String> {
        let entries = std::fs::read_dir(path).map_err(|e| e.to_string())?;
        let mut result = Vec::new();
        for entry in entries {
            let entry = entry.map_err(|e| e.to_string())?;
            if let Some(name) = entry.file_name().to_str() {
                result.push(name.to_string());
            }
        }
        Ok(result)
    }

    /// Append text to a file, matching Aver's Disk.appendText semantics.
    pub fn append_text(path: &str, content: &str) -> Result<(), String> {
        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_err(|e| e.to_string())?;
        file.write_all(content.as_bytes())
            .map_err(|e| e.to_string())
    }

    /// Display a value in Aver format (used by Console.print and string interpolation).
    /// This is the primary display function — it formats values the way Aver does.
    pub fn aver_display<T: AverDisplay>(val: &T) -> String {
        val.aver_display()
    }

    /// Trait for Aver-compatible display.
    pub trait AverDisplay {
        fn aver_display(&self) -> String;
        /// Inner display — strings get quoted inside containers.
        fn aver_display_inner(&self) -> String {
            self.aver_display()
        }
    }

    impl AverDisplay for i64 {
        fn aver_display(&self) -> String { self.to_string() }
    }

    impl AverDisplay for f64 {
        fn aver_display(&self) -> String { self.to_string() }
    }

    impl AverDisplay for String {
        fn aver_display(&self) -> String { self.clone() }
        fn aver_display_inner(&self) -> String { format!("\"{}\"", self) }
    }

    impl AverDisplay for bool {
        fn aver_display(&self) -> String {
            if *self { "true".to_string() } else { "false".to_string() }
        }
    }

    impl AverDisplay for () {
        fn aver_display(&self) -> String { "()".to_string() }
    }

    impl<T: AverDisplay, E: AverDisplay> AverDisplay for Result<T, E> {
        fn aver_display(&self) -> String {
            match self {
                Ok(v) => format!("Result.Ok({})", v.aver_display_inner()),
                Err(e) => format!("Result.Err({})", e.aver_display_inner()),
            }
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<T: AverDisplay> AverDisplay for Option<T> {
        fn aver_display(&self) -> String {
            match self {
                Some(v) => format!("Option.Some({})", v.aver_display_inner()),
                None => "Option.None".to_string(),
            }
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<T: AverDisplay> AverDisplay for Vec<T> {
        fn aver_display(&self) -> String {
            let parts: Vec<String> = self.iter().map(|x| x.aver_display_inner()).collect();
            format!("[{}]", parts.join(", "))
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<K: AverDisplay + Eq + std::hash::Hash + Ord, V: AverDisplay> AverDisplay for std::collections::HashMap<K, V> {
        fn aver_display(&self) -> String {
            let mut keys: Vec<&K> = self.keys().collect();
            keys.sort();
            let parts: Vec<String> = keys.iter()
                .map(|k| format!("{}: {}", k.aver_display_inner(), self[*k].aver_display_inner()))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<A: AverDisplay, B: AverDisplay> AverDisplay for (A, B) {
        fn aver_display(&self) -> String {
            format!("({}, {})", self.0.aver_display_inner(), self.1.aver_display_inner())
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<A: AverDisplay, B: AverDisplay, C: AverDisplay> AverDisplay for (A, B, C) {
        fn aver_display(&self) -> String {
            format!("({}, {}, {})", self.0.aver_display_inner(), self.1.aver_display_inner(), self.2.aver_display_inner())
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }
}

fn main() {
    let o = Some("x".to_string());
    let d = o.clone().unwrap_or("y".to_string());
    println!("{}", aver_rt::aver_display(&o));
}
