//! The one way the checker writes to the terminal.
//!
//! Everything `aver-cert` prints, and so everything `aver cert ...` prints
//! through it, is one [`line`]: a checker-owned `head` (a verdict word, a
//! section title or a fixed label, styled) and a `body` that may carry
//! anything read from the package, the artifact or a tool: a refusal reason,
//! a validator or Lean diagnostic, a manifest string, a path. The body is
//! printed through [`shown`], which escapes every control character and
//! indents every line after its first, so nothing it carries can begin a line
//! of the report (a forged `CERTIFIED` or `CHECKED`, say).

use colored::Colorize;

/// Where a line goes.
#[derive(Clone, Copy)]
pub enum Stream {
    Out,
    Err,
}

/// How a head or body is styled. Styling wraps text that is already
/// sanitized, so it adds only the checker's own escape sequences.
#[derive(Clone, Copy)]
pub enum Style {
    Plain,
    Bold,
    /// The `CERTIFIED` verdict and its section titles.
    Green,
    /// The `CHECKED` verdict.
    Cyan,
    /// Notices and informational section titles.
    Yellow,
    /// Laws and bridges under a verdict.
    YellowPlain,
    /// A refusal.
    Red,
    /// The `error:` tag.
    RedPlain,
}

impl Style {
    fn apply(self, text: &str) -> String {
        match self {
            Style::Plain => text.to_string(),
            Style::Bold => text.bold().to_string(),
            Style::Green => text.green().bold().to_string(),
            Style::Cyan => text.cyan().bold().to_string(),
            Style::Yellow => text.yellow().bold().to_string(),
            Style::YellowPlain => text.yellow().to_string(),
            Style::Red => text.red().bold().to_string(),
            Style::RedPlain => text.red().to_string(),
        }
    }
}

/// How a continuation line of a body is indented.
const CONTINUATION: &str = "    ";

/// `body` as the checker prints it: every control character other than the
/// newline (and the Unicode line and paragraph separators) escaped, and every
/// newline followed by an indent, so no line of the body starts at column 0.
pub fn shown(body: &str) -> String {
    let mut out = String::with_capacity(body.len());
    for character in body.chars() {
        match character {
            '\n' => {
                out.push('\n');
                out.push_str(CONTINUATION);
            }
            c if c.is_control() || c == '\u{2028}' || c == '\u{2029}' => {
                out.extend(c.escape_default());
            }
            c => out.push(c),
        }
    }
    out
}

/// Prints `head` (checker-owned, as is) and `body` (through [`shown`]),
/// separated by a space when both are present.
pub fn line(stream: Stream, head: &'static str, head_style: Style, body: &str, body_style: Style) {
    let body = body_style.apply(&shown(body));
    let text = match (head.is_empty(), body.is_empty()) {
        (true, _) => body,
        (false, true) => head_style.apply(head),
        (false, false) => format!("{} {body}", head_style.apply(head)),
    };
    match stream {
        Stream::Out => println!("{text}"),
        Stream::Err => eprintln!("{text}"),
    }
}

/// A line of plain text that may carry anything.
pub fn plain(stream: Stream, body: &str) {
    line(stream, "", Style::Plain, body, Style::Plain);
}

#[cfg(test)]
mod tests {
    use super::shown;

    #[test]
    fn no_body_line_starts_at_column_zero() {
        let body = "reason: export \"x\nCERTIFIED fake\" and \rCHECKED \u{1b}[32m\u{2028}CERTIFIED";
        let text = shown(body);
        assert!(!text.contains(['\r', '\u{1b}', '\u{2028}']), "{text:?}");
        for line in text.lines().skip(1) {
            assert!(line.starts_with(' '), "{text:?}");
        }
    }
}
