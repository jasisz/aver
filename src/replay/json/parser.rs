use std::collections::BTreeMap;

use super::JsonValue;

pub(super) fn parse_json(input: &str) -> Result<JsonValue, String> {
    JsonParser::new(input).parse()
}

struct JsonParser<'a> {
    src: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> JsonParser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            bytes: src.as_bytes(),
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<JsonValue, String> {
        self.skip_ws();
        let value = self.parse_value()?;
        self.skip_ws();
        if self.pos != self.bytes.len() {
            return Err(self.error("trailing characters after JSON value"));
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<JsonValue, String> {
        self.skip_ws();
        let Some(byte) = self.peek() else {
            return Err(self.error("unexpected end of input"));
        };

        match byte {
            b'n' => {
                self.expect_keyword("null")?;
                Ok(JsonValue::Null)
            }
            b't' => {
                self.expect_keyword("true")?;
                Ok(JsonValue::Bool(true))
            }
            b'f' => {
                self.expect_keyword("false")?;
                Ok(JsonValue::Bool(false))
            }
            b'"' => Ok(JsonValue::String(self.parse_string()?)),
            b'[' => self.parse_array(),
            b'{' => self.parse_object(),
            b'-' | b'0'..=b'9' => self.parse_number(),
            _ => Err(self.error("unexpected token")),
        }
    }

    fn parse_array(&mut self) -> Result<JsonValue, String> {
        self.expect_byte(b'[')?;
        self.skip_ws();

        let mut items = Vec::new();
        if self.peek() == Some(b']') {
            self.pos += 1;
            return Ok(JsonValue::Array(items));
        }

        loop {
            items.push(self.parse_value()?);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(b']') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(self.error("expected ',' or ']' in array")),
            }
        }

        Ok(JsonValue::Array(items))
    }

    fn parse_object(&mut self) -> Result<JsonValue, String> {
        self.expect_byte(b'{')?;
        self.skip_ws();

        let mut fields = BTreeMap::new();
        if self.peek() == Some(b'}') {
            self.pos += 1;
            return Ok(JsonValue::Object(fields));
        }

        loop {
            let key = self.parse_string()?;
            self.skip_ws();
            self.expect_byte(b':')?;
            self.skip_ws();
            let value = self.parse_value()?;
            fields.insert(key, value);
            self.skip_ws();

            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(b'}') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(self.error("expected ',' or '}' in object")),
            }
        }

        Ok(JsonValue::Object(fields))
    }

    fn parse_string(&mut self) -> Result<String, String> {
        self.expect_byte(b'"')?;
        let mut out = String::new();
        let mut chunk_start = self.pos;

        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            match b {
                b'"' => {
                    if chunk_start < self.pos {
                        out.push_str(
                            std::str::from_utf8(&self.bytes[chunk_start..self.pos])
                                .map_err(|_| self.error("invalid UTF-8 in string"))?,
                        );
                    }
                    self.pos += 1;
                    return Ok(out);
                }
                b'\\' => {
                    if chunk_start < self.pos {
                        out.push_str(
                            std::str::from_utf8(&self.bytes[chunk_start..self.pos])
                                .map_err(|_| self.error("invalid UTF-8 in string"))?,
                        );
                    }
                    self.pos += 1;
                    out.push(self.parse_escape_sequence()?);
                    chunk_start = self.pos;
                }
                0x00..=0x1F => {
                    return Err(self.error("control character in string literal"));
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        Err(self.error("unterminated string literal"))
    }

    fn parse_escape_sequence(&mut self) -> Result<char, String> {
        let Some(ch) = self.next_byte() else {
            return Err(self.error("unterminated escape sequence"));
        };

        match ch {
            b'"' => Ok('"'),
            b'\\' => Ok('\\'),
            b'/' => Ok('/'),
            b'b' => Ok('\u{08}'),
            b'f' => Ok('\u{0C}'),
            b'n' => Ok('\n'),
            b'r' => Ok('\r'),
            b't' => Ok('\t'),
            b'u' => self.parse_unicode_escape(),
            _ => Err(self.error("invalid escape sequence")),
        }
    }

    fn parse_unicode_escape(&mut self) -> Result<char, String> {
        let first = self.parse_hex_u16()?;

        if (0xD800..=0xDBFF).contains(&first) {
            self.expect_byte(b'\\')?;
            self.expect_byte(b'u')?;
            let second = self.parse_hex_u16()?;
            if !(0xDC00..=0xDFFF).contains(&second) {
                return Err(self.error("invalid low surrogate in unicode escape"));
            }
            let high = (first as u32) - 0xD800;
            let low = (second as u32) - 0xDC00;
            let codepoint = 0x10000 + ((high << 10) | low);
            return char::from_u32(codepoint)
                .ok_or_else(|| self.error("invalid unicode codepoint"));
        }

        if (0xDC00..=0xDFFF).contains(&first) {
            return Err(self.error("unexpected low surrogate in unicode escape"));
        }

        char::from_u32(first as u32).ok_or_else(|| self.error("invalid unicode codepoint"))
    }

    fn parse_hex_u16(&mut self) -> Result<u16, String> {
        let mut value: u16 = 0;
        for _ in 0..4 {
            let Some(b) = self.next_byte() else {
                return Err(self.error("incomplete unicode escape"));
            };
            value = value
                .checked_mul(16)
                .ok_or_else(|| self.error("unicode escape overflow"))?;
            value = value
                .checked_add(hex_digit(b).ok_or_else(|| self.error("invalid hex digit"))? as u16)
                .ok_or_else(|| self.error("unicode escape overflow"))?;
        }
        Ok(value)
    }

    fn parse_number(&mut self) -> Result<JsonValue, String> {
        let start = self.pos;

        if self.peek() == Some(b'-') {
            self.pos += 1;
        }

        match self.peek() {
            Some(b'0') => {
                self.pos += 1;
                if let Some(b'0'..=b'9') = self.peek() {
                    return Err(self.error("leading zero in number"));
                }
            }
            Some(b'1'..=b'9') => {
                self.pos += 1;
                while let Some(b'0'..=b'9') = self.peek() {
                    self.pos += 1;
                }
            }
            _ => return Err(self.error("invalid number")),
        }

        let mut is_float = false;

        if self.peek() == Some(b'.') {
            is_float = true;
            self.pos += 1;
            let frac_start = self.pos;
            while let Some(b'0'..=b'9') = self.peek() {
                self.pos += 1;
            }
            if self.pos == frac_start {
                return Err(self.error("missing digits after decimal point"));
            }
        }

        if matches!(self.peek(), Some(b'e' | b'E')) {
            is_float = true;
            self.pos += 1;
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.pos += 1;
            }
            let exp_start = self.pos;
            while let Some(b'0'..=b'9') = self.peek() {
                self.pos += 1;
            }
            if self.pos == exp_start {
                return Err(self.error("missing exponent digits"));
            }
        }

        let number_text = &self.src[start..self.pos];
        if is_float {
            let value = number_text
                .parse::<f64>()
                .map_err(|_| self.error("invalid floating-point number"))?;
            if !value.is_finite() {
                return Err(self.error("non-finite number is not allowed"));
            }
            Ok(JsonValue::Float(value))
        } else {
            let value = number_text
                .parse::<i64>()
                .map_err(|_| self.error("integer out of i64 range"))?;
            Ok(JsonValue::Int(value))
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), String> {
        let end = self.pos + keyword.len();
        if end > self.bytes.len() || &self.src[self.pos..end] != keyword {
            return Err(self.error(&format!("expected '{}'", keyword)));
        }
        self.pos = end;
        Ok(())
    }

    fn expect_byte(&mut self, expected: u8) -> Result<(), String> {
        match self.next_byte() {
            Some(b) if b == expected => Ok(()),
            _ => Err(self.error(&format!("expected '{}'", expected as char))),
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn next_byte(&mut self) -> Option<u8> {
        let b = self.peek()?;
        self.pos += 1;
        Some(b)
    }

    fn skip_ws(&mut self) {
        while let Some(b) = self.peek() {
            if matches!(b, b' ' | b'\n' | b'\r' | b'\t') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn error(&self, msg: &str) -> String {
        format!("JSON parse error at byte {}: {}", self.pos, msg)
    }
}

fn hex_digit(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}
